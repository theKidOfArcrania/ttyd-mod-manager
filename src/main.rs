#![feature(byte_slice_trim_ascii)]
#![feature(error_generic_member_access)]
#![feature(lazy_cell)]
use std::{
    collections::{BTreeSet, HashMap},
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::bail;
use clap::{Subcommand, Parser};

extern crate num_derive;

mod error;
mod evt;
mod gen;
mod msg;
mod sym;
mod reader;
mod rel;
mod utils;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    /// The root directory for the mod
    #[arg(short = 'r', long = "mod-root")]
    mod_root: String,

    /// The game path from the ISO root used to locate all the files
    ///
    /// Defaults to P-G8ME/files
    #[arg(short = 'p', default_value = "P-G8ME/files")]
    game_path: String,

    /// The language used for the game
    ///
    /// Defaults to US
    #[arg(short = 'l', default_value = "US")]
    lang: String,
}

#[derive(Subcommand)]
enum Command {
    /// Build the mod into the mod directory which can then be opened in
    /// dolphin to be packed into an ISO file
    Build,
    /// Manage messages in this mod
    Message {
        #[command(subcommand)]
        cmd: Option<MessageCommand>,
    },
    /// Manage rel files
    Rel {
        /// The rel file to process
        #[arg(short = 'f', long)]
        file: String,
        #[command(subcommand)]
        cmd: RelCommands,
    },
}

#[derive(Subcommand)]
enum MessageCommand {
    #[command(subcommand)]
    Files(MessageFilesCommands)
}

#[derive(Subcommand)]
enum MessageFilesCommands {
    /// List all available message files
    List,
    Select {
        /// The file id for the particular message
        id: String,
    }
}

#[derive(Subcommand)]
enum RelCommands {
    /// Dump the headers of the REL file
    Headers,
    /// Dump the scripts of the REL file
    Scripts {
        /// File path to the symbol csv database for the game
        symdb: String,
    },
    /// Dump the C source of the REL file
    Source {
        /// File path to the symbol csv database for the game
        symdb: String,
        /// Path to export source files to
        outdir: String,
    },
}

#[allow(dead_code)]
enum HandlerType {
    ByByte(Box<dyn Fn(Option<Vec<u8>>, Vec<u8>) -> anyhow::Result<Vec<u8>>>),
    ByString(Box<dyn Fn(Option<String>, String) -> anyhow::Result<String>>),
    ByFile(Box<dyn for<'a> Fn(Option<&'a Path>, &'a Path, &'a Path) -> anyhow::Result<()>>)
}

struct PatchHandler {
    ext_pattern: String,
    ext_repl: String,
    handler: HandlerType,
}

impl PatchHandler {
    fn new(ext_pattern: String, ext_repl: String, handler: HandlerType) -> Self {
        Self { ext_pattern, ext_repl, handler }
    }
}

fn copy_dir_all(
    src: impl AsRef<Path>,
    dst: impl AsRef<Path>,
    patch_handlers: &[PatchHandler],
) -> anyhow::Result<()> {
    fs::create_dir_all(&dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        if ty.is_dir() {
            copy_dir_all(
                entry.path(),
                dst.as_ref().join(entry.file_name()),
                patch_handlers,
            )?;
        } else {
            let filename = entry
                .file_name()
                .to_string_lossy()
                .to_string();
            for hdr in patch_handlers {
                if let Some(base) = filename.strip_suffix(&hdr.ext_pattern) {
                    // We found a patch handler match
                    let mut new_filename = base.to_string();
                    new_filename.push_str(&hdr.ext_repl);
                    let output_path = dst.as_ref().join(new_filename);
                    let base_path = if output_path.exists() {
                        Some(output_path.as_path())
                    } else {
                        None
                    };

                    let provide_ctx = |e: anyhow::Error| {
                        e.context(entry.path().to_string_lossy().to_string())
                    };

                    return match &hdr.handler {
                        HandlerType::ByByte(hdr) => {
                            let base = match base_path {
                                None => None,
                                Some(path) => Some(fs::read(path)?)
                            };
                            let patch = fs::read(entry.path())?;
                            fs::write(
                                output_path,
                                hdr(base, patch).map_err(provide_ctx)?,
                            )?;
                            Ok(())
                        }
                        HandlerType::ByString(hdr) => {
                            let base = match base_path {
                                None => None,
                                Some(path) => Some(fs::read_to_string(path)?)
                            };
                            let patch = fs::read_to_string(entry.path())?;
                            fs::write(
                                output_path,
                                hdr(base, patch).map_err(provide_ctx)?,
                            )?;
                            Ok(())
                        }
                        HandlerType::ByFile(hdr) => {
                            hdr(base_path, &entry.path(), &output_path)
                        }
                    };
                }
            }

            // Default case
            fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}

struct Env {
    root_dir: PathBuf,
    base_dir: PathBuf,
    mod_dir: PathBuf,
    patched_dir: PathBuf,

    lang: String,
    game_path: String,
}

impl Env {
    pub fn new(cli: &Cli) -> anyhow::Result<Self> {
        let root_dir = Path::new(&cli.mod_root).to_path_buf();
        let base_dir = root_dir.join("base");
        let mod_dir = root_dir.join("mod");
        let patched_dir = root_dir.join("patch");
        let game_dir = base_dir.join(&cli.game_path);

        if !base_dir.is_dir() {
            bail!("base directory not found");
        }

        if !mod_dir.is_dir() {
            bail!("mod directory not found");
        }

        if !game_dir.is_dir() {
            bail!(
                "base game directory not found. Are you sure it is at {}",
                &cli.game_path,
            );
        }

        if !game_dir.join("msg").join(&cli.lang).is_dir() {
            bail!(
                "unable to find messages for language {}",
                &cli.lang,
            );
        }

        Ok(Self {
            root_dir,
            base_dir,
            mod_dir,
            patched_dir,
            game_path: cli.game_path.clone(),
            lang: cli.lang.clone(),
        })
    }

    pub fn base_dir(&self) -> &Path {
        &self.base_dir
    }

    pub fn mod_dir(&self) -> &Path {
        &self.mod_dir
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub fn patched_dir(&self) -> &Path {
        &self.patched_dir
    }

    pub fn game_dir(&self) -> PathBuf {
        self.base_dir.join(&self.game_path)
    }

    pub fn msg_dir(&self) -> PathBuf {
        self.game_dir().join("msg").join(&self.lang)
    }

}

fn message_cmd(env: &Env, cmd: MessageCommand) -> Result<(), anyhow::Error> {
    match cmd {
        MessageCommand::Files(cmd) => match cmd {
            MessageFilesCommands::List => {
                let mut files = BTreeSet::new();
                println!("{}", env.msg_dir().to_string_lossy());
                for file in fs::read_dir(env.msg_dir())? {
                    files.insert(file?.file_name().to_string_lossy().to_string());
                }
                // TODO: list files that would be patched
                for file in files {
                    println!("{}", file.strip_suffix(".txt").unwrap_or(&file));
                }
                Ok(())
            },
            MessageFilesCommands::Select { id: _id } => todo!(),
        },
    }
}

fn build_cmd(env: &Env) -> Result<(), anyhow::Error> {
    if env.patched_dir().exists() {
        if env.patched_dir().is_dir() {
            fs::remove_dir_all(env.patched_dir())?;
        } else {
            fs::remove_file(env.patched_dir())?;
        }
    }

    copy_dir_all(env.base_dir(), env.patched_dir(), &[])?;
    copy_dir_all(env.mod_dir(), env.patched_dir(), &[
        PatchHandler::new(
            ".txt.msgpatch".into(),
            ".txt".into(),
            HandlerType::ByByte(Box::new(&msg::patch_msgfile)),
        ),
    ])?;

    Ok(())
}

fn main() -> Result<(), anyhow::Error> {
    let cli = Cli::parse();
    let env = Env::new(&cli)?;
    match cli.command {
        Command::Build => build_cmd(&env),
        Command::Message { cmd } => {
            match cmd {
                Some(cmd) => message_cmd(&env, cmd),
                None => {
                    #[derive(Parser)]
                    struct MessageParser {
                        #[command(subcommand)]
                        cmd: MessageCommand,
                    }

                    // TODO:
                    //
                    Ok(())
                }
            }
        },
        Command::Rel { file, cmd } => {
            let file_parts = file
                .rsplitn(2, '/')
                .next()
                .expect("should be at least one part")
                .split_once(".");
            let area_name = match file_parts {
                Some((name, "rel")) => name,
                _ => {
                    println!("Unable to extract area name from file!");
                    "aaa"
                }
            };
            let data = fs::read(env.root_dir().join(file.clone()))?;
            let rel = rel::RelFile::new(&data);

            match cmd {
                RelCommands::Headers => rel.dump_headers(),
                RelCommands::Scripts { symdb } => {
                    let mut overlay = rel::RelocOverlay::new(&rel);
                    overlay.resolve_relocations(&HashMap::new(), true)?;

                    let mut area_map = HashMap::new();
                    area_map.insert(area_name.into(), rel.header().id.get());
                    let symdb = sym::SymbolDatabase::new(
                        area_map,
                        sym::RawSymtab::from_reader(
                            File::open(symdb)?
                        )?
                    );
                    let parser = evt::EvtParser::new(&overlay);
                    parser.search_evt_scripts(&symdb)?;
                    parser.add_from_symdb(&symdb)?;
                    parser.dump_scripts(&symdb);
                }
                RelCommands::Source { symdb, outdir } => {
                    let outdir = PathBuf::from(outdir);
                    fs::create_dir_all(&outdir)?;

                    let mut overlay = rel::RelocOverlay::new(&rel);
                    overlay.resolve_relocations(&HashMap::new(), true)?;

                    let mut area_map = HashMap::new();
                    let area_id = rel.header().id.get();
                    area_map.insert(area_name.into(), area_id);
                    let symdb = sym::SymbolDatabase::new(
                        area_map,
                        sym::RawSymtab::from_reader(
                            File::open(symdb)?
                        )?
                    );
                    
                    let mut codes = HashMap::new();
                    let mut headers = BTreeSet::new();
                    for s in symdb.rel_iter(area_id) {
                        let mut file = format!("{}.c", area_name);
                        let mut file_include = format!("{}.h", area_name);
                        for ns in s.namespace.split(" ") {
                            if ns.ends_with(".o") {
                                file = format!("{}.c", &ns[..ns.len() - 2]);
                                file_include = format!("{}.h", &ns[..ns.len() - 2]);
                                break;
                            }
                        }
                        let def = gen::generate_line(&overlay, &symdb, s)?;
                        let code = codes.entry(file).or_insert_with(String::new);
                        code.push_str(&def.definition);
                        code.push_str("\n");

                        if let Some(decl) = &def.declare {
                            headers.insert(format!(
                                "#include \"{file_include}\"\n"
                            ));
                            let code = codes
                                .entry(file_include)
                                .or_insert_with(String::new);
                            code.push_str(decl);
                            code.push_str("\n");
                        }
                    }

                    for (file, code) in codes.into_iter() {
                        let is_source = file.ends_with(".c");
                        let mut file = File::create(outdir.join(file))?;
                        if is_source {
                            for inc_line in &headers {
                                file.write_all(inc_line.as_bytes())?;
                            }
                            if headers.is_empty() {
                                file.write_all(b"\n")?;
                            }
                        }
                        file.write_all(code.as_bytes())?;
                    }
                }
            }
            Ok(())
        },
    }
}
