#![feature(iter_intersperse)]
#![feature(byte_slice_trim_ascii)]
#![feature(lazy_cell)]
#![feature(generic_const_exprs)]
#![feature(error_generic_member_access)]
#![feature(strict_overflow_ops)]
#![feature(const_trait_impl)]
#![allow(incomplete_features)]
use std::{
    borrow::Cow,
    collections::{hash_map as hm, BTreeMap, BTreeSet, HashMap},
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::bail;
use clap::{Parser, Subcommand};

extern crate num_derive;

mod bpatch;
mod code;
mod clsdata;
mod dol;
mod evt;
mod gen;
mod msg;
mod reader;
mod rel;
mod sym;
mod utils;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    /// The root directory for the mod
    #[arg(short = 'r', long = "mod-root")]
    mod_root: String,

    /// Load base mod information. By default we don't do it.
    #[arg(short = 'L', long)]
    load_base_info: bool,

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
    /// Fixes symbols
    FixSymbols {
        /// The symbol database to work with
        symdb: String,
        /// Whether to not fatally exit after one error of symbols failing to
        /// parse. Defaults to false
        #[arg(long)]
        exit_fail_parse: bool
    },
    /// Manage rel files
    Rel {
        /// Area name
        #[arg(short, long)]
        area_name: Option<String>,
        /// The rel file to process
        #[arg(short = 'f', long)]
        file: String,
        #[command(subcommand)]
        cmd: RelCommands,
    },
    /// Make a rel file from an input elf binary
    MakeRel {
        /// Area name
        #[arg(short, long)]
        area_name: Option<String>,
        /// Output REL file
        #[arg(short = 'o', long)]
        output: String,
        /// The symbol database to extract information from
        #[arg(long)]
        symdb: String,
        /// The id used for this REL file.
        #[arg(long)]
        id: Option<u32>,
        /// Input ELF binary
        input: String,
    }
}

#[derive(Subcommand)]
enum MessageCommand {
    #[command(subcommand)]
    Files(MessageFilesCommands),
}

#[derive(Subcommand)]
enum MessageFilesCommands {
    /// List all available message files
    List,
    Select {
        /// The file id for the particular message
        id: String,
    },
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
    /// Convert the REL file into an ELF file
    Elf {
        /// File path to the symbol csv database for the game
        symdb: String,
        /// Path to elf file to write to
        out: String,
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
    ByFile(
        Box<
            dyn for<'a> Fn(
                Option<&'a Path>,
                &'a Path,
                &'a Path,
            ) -> anyhow::Result<()>,
        >,
    ),
}

struct PatchHandler {
    ext_pattern: String,
    ext_repl: String,
    handler: HandlerType,
}

impl PatchHandler {
    fn new(
        ext_pattern: String,
        ext_repl: String,
        handler: HandlerType,
    ) -> Self {
        Self {
            ext_pattern,
            ext_repl,
            handler,
        }
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
            let filename = entry.file_name().to_string_lossy().to_string();
            let mut handled = false;
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

                    match &hdr.handler {
                        HandlerType::ByByte(hdr) => {
                            let base = match base_path {
                                None => None,
                                Some(path) => Some(fs::read(path)?),
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
                                Some(path) => Some(fs::read_to_string(path)?),
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
                    }?;
                    handled = true;
                    break;
                }
            }

            // Default case
            if !handled {
                fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
            }
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
            bail!("unable to find messages for language {}", &cli.lang,);
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
                    files.insert(
                        file?.file_name().to_string_lossy().to_string(),
                    );
                }
                // TODO: list files that would be patched
                for file in files {
                    println!("{}", file.strip_suffix(".txt").unwrap_or(&file));
                }
                Ok(())
            }
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
    copy_dir_all(
        env.mod_dir(),
        env.patched_dir(),
        &[PatchHandler::new(
            ".txt.msgpatch".into(),
            ".txt".into(),
            HandlerType::ByByte(Box::new(&msg::patch_msgfile)),
        ), PatchHandler::new(
            ".dol.binpatch".into(),
            ".dol".into(),
            HandlerType::ByFile(Box::new(&bpatch::patch_dol)),
        )],
    )?;

    Ok(())
}

fn get_rel_file<'r>(
    env: &Env,
    rel_files: &'r mut HashMap<String, rel::RelocFileOverlay>,
    area_name: &str,
) -> Result<&'r rel::RelocFileOverlay, anyhow::Error> {
    Ok(match rel_files.entry(area_name.to_string()) {
        hm::Entry::Occupied(ent) => ent.into_mut(),
        hm::Entry::Vacant(ent) => {
            let data = fs::read(env.base_dir().join(format!(
                "P-G8ME/files/rel/{}.rel",
                area_name,
            )))?;
            let rel = rel::RelFile::new_owned(data);
            ent.insert(rel::RelocFileOverlay::new_from(rel))
        }
    })
}

const SCRIPT_TAG: [u8;8] = [1, 0, 0, 0, 2, 0, 0, 0];

fn main() -> Result<(), anyhow::Error> {
    let cli = Cli::parse();
    let env = Env::new(&cli)?;

    let mut area_map = HashMap::new();
    let mut area_map_names = HashMap::new();
    if cli.load_base_info {
        for file in env.base_dir.join("P-G8ME/files/rel/").read_dir()? {
            let file = file?;
            let name = file.file_name().to_string_lossy().to_string();
            if !name.ends_with(".rel") {
                continue;
            }

            let data = fs::read(file.path())?;
            let rel = rel::RelFile::new(&data);
            let id = rel.header().id.get();
            let base = &name[..name.len() - 4];
            area_map.insert(base.to_string(), id);
            if let Some(conflict_name) = area_map_names
                .insert(id, base.to_string())
            {
                eprintln!("WARNING: {base} conflicts with {conflict_name} on id {id}");
            }
        }
    }

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
        }
        Command::FixSymbols { symdb: symdb_file, exit_fail_parse } => {
            let mut raw_symtab = sym::RawSymtab::from_reader_raw(
                File::open(&symdb_file)?
            )?;

            let parsed_symtab = sym::RawSymtab::from_reader(
                File::open(&symdb_file)?
            )?;
            let symdb = sym::SymbolDatabase::new(
                area_map.clone(),
                parsed_symtab.clone(),
            );

            let dol_path = env.base_dir().join("P-G8ME/sys/main.dol");
            let dol_file = dol::DolFile::from_reader(File::open(dol_path)?)?;
            let mut rel_files = HashMap::new();
            for sym in &mut raw_symtab {
                let saddr = if &sym.area == "_main" {
                    sym::SymAddr::Dol(sym.ram_addr.unwrap())
                } else {
                    if let Some(file_id) = area_map.get(&sym.area) {
                        sym::SymAddr::Rel(*file_id, sym.section_addr())
                    } else {
                        continue;
                    }
                };
                let mut parsed_sym = symdb.get(saddr).expect("Should exist");
                if sym.value_type != sym::DataType::default() {
                    continue;
                }
                if sym.sec_type.is_bss() {
                    continue;
                }
                let sdata = match saddr {
                    sym::SymAddr::Dol(addr) => {
                        match dol_file.lookup_section_data(addr) {
                            None => Cow::Borrowed(&[] as &[u8]),
                            Some(Cow::Owned(mut o)) => {
                                o.truncate(sym.size as usize);
                                Cow::Owned(o)
                            }
                            Some(Cow::Borrowed(b)) => {
                                Cow::Borrowed(&b[..sym.size as usize])
                            }
                        }
                    }
                    sym::SymAddr::Rel(_, saddr) => {
                        let overlay = get_rel_file(&env, &mut rel_files, &sym.area)?
                            .overlay();
                        // TODO: this is very slow because we are reading byte
                        // by byte, but don't really care since this isn't
                        // running by default in production code
                        let mut ret = Vec::with_capacity(sym.size as usize);
                        for i in 0..sym.size {
                            ret.push(match overlay.read(saddr + i)? {
                                rel::Symbol::Value(v) => v,
                                _ => 0u8,
                            })
                        }

                        Cow::Owned(ret)
                    }
                };
                let mut assumed_data_tp = sym::DataType::Simple(
                    sym::SimpleType::Function
                );
                if !sym.sec_type.is_exec() {
                    let data = sdata.as_ref();
                    if data.len() % 4 == 0 && data.ends_with(&SCRIPT_TAG) {
                        eprintln!(
                            "{} entry's type assumed to be evt.",
                            sym.name,
                        );
                        assumed_data_tp = sym::DataType::Simple(sym::SimpleType::Evt);
                    } else if data == [0xbf, 0x80, 0, 0, 0x3f, 0x80, 0, 0] ||
                        data == [0x3f, 0x80, 0, 0, 0xbf, 0x80, 0, 0] {
                            assumed_data_tp = sym::DataType::Class(
                                clsdata::ClsDataType::FloatPair
                            );
                    } else {
                        eprintln!(
                            "WARNING: {} entry's type is unknown",
                            sym.name,
                        );
                        continue;
                    }
                    parsed_sym.value_type = assumed_data_tp;
                }
                let res = match saddr {
                    sym::SymAddr::Dol(_) => {
                        gen::Data::read_dol(&dol_file, &parsed_sym, &symdb)
                    }
                    sym::SymAddr::Rel(_, _) => {
                        let overlay = get_rel_file(&env, &mut rel_files, &sym.area)?
                            .overlay();
                        gen::Data::read_rel(overlay, &parsed_sym, &symdb)
                    }
                };
                match res {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!(
                            "****FAILED: {e}: {}: {:?}",
                            sym.name,
                            sdata,
                        );
                        if exit_fail_parse {
                            return Err(e.into());
                        }
                        continue;
                    }
                }
                sym.value_type = assumed_data_tp;
            }

            raw_symtab.write_to(File::create(symdb_file)?)?;
            Ok(())
        }
        Command::MakeRel { area_name, output, symdb, input, id } => {
            let area_name = area_name.as_ref().map(String::as_str).or_else(||
                output.rsplitn(2, '/')
                    .next()
                    .expect("should be at least one part")
                    .split_once(".")
                    .and_then(|(name, ext)| {
                        if ext == "rel" {
                            Some(name)
                        } else {
                            None
                        }
                    })
            ).ok_or_else(|| anyhow::anyhow!("Unable to extract area name from file!"))?;
            let symdb = sym::SymbolDatabase::new(
                area_map.clone(),
                sym::RawSymtab::from_reader(File::open(symdb)?)?,
            );
            let id = id.or_else(|| area_map.get(area_name).copied())
                .ok_or_else(|| anyhow::anyhow!(
                    "Unable to retrieve id for {area_name}. Maybe try \
                    specifying it explicitly?"
                ))?;

            let elfdata = fs::read(input)?;
            let rel = rel::RelFile::from_elf(id, &elfdata, &symdb)?;

            fs::write(output, rel.as_bytes())?;
            Ok(())
        }
        Command::Rel { area_name, file, cmd } => {
            let area_name = area_name.as_ref().map(String::as_str).or_else(||
                file.rsplitn(2, '/')
                    .next()
                    .expect("should be at least one part")
                    .split_once(".")
                    .and_then(|(name, ext)| {
                        if ext == "rel" {
                            Some(name)
                        } else {
                            None
                        }
                    })
            ).ok_or_else(|| anyhow::anyhow!("Unable to extract area name from file!"))?;
            let data = fs::read(env.root_dir().join(file.clone()))?;
            let rel = rel::RelFile::new(&data);

            match cmd {
                RelCommands::Headers => rel.dump_headers(),
                RelCommands::Scripts { symdb } => {
                    let mut overlay = rel::RelocOverlay::new(&rel);
                    overlay.resolve_relocations(&HashMap::new(), true)?;

                    area_map.insert(area_name.into(), rel.header().id.get());
                    let symdb = sym::SymbolDatabase::new(
                        area_map,
                        sym::RawSymtab::from_reader(File::open(symdb)?)?,
                    );
                    let parser = evt::EvtParser::new(&overlay);
                    parser.search_evt_scripts(&symdb)?;
                    parser.add_from_symdb(&symdb)?;
                    parser.dump_scripts(&symdb, &sym::StringsMap::new());
                }
                RelCommands::Elf { symdb, out } => {
                    let area_id = rel.header().id.get();
                    area_map.insert(area_name.into(), area_id);
                    let symdb = sym::SymbolDatabase::new(
                        area_map,
                        sym::RawSymtab::from_reader(File::open(symdb)?)?,
                    );
                    let elf = rel.to_elf(&symdb)?;
                    let mut out_file = File::create(out)?;
                    out_file.write_all(&elf)?;
                }
                RelCommands::Source { symdb, outdir } => {
                    let outdir = PathBuf::from(outdir);
                    fs::create_dir_all(&outdir)?;

                    let mut overlay = rel::RelocOverlay::new(&rel);
                    overlay.resolve_relocations(&HashMap::new(), true)?;

                    let area_id = rel.header().id.get();
                    area_map.insert(area_name.into(), area_id);
                    let symdb = sym::SymbolDatabase::new(
                        area_map,
                        sym::RawSymtab::from_reader(File::open(symdb)?)?,
                    );

                    let mut strings = sym::StringsMap::new();
                    for ent in symdb.rel_iter(area_id) {
                        if ent.value_type == sym::DataType::Simple(
                            sym::SimpleType::String) && ent.sec_name.is_ro()
                        {
                            strings.insert(
                                sym::SymAddr::Rel(area_id, ent.section_addr()),
                                match gen::Data::read_rel(&overlay, ent, &symdb)? {
                                    gen::Data::String(val) => {
                                        val.0
                                    }
                                    _ => panic!("should parse to string"),
                                }
                            );
                        }
                    }

                    let mut codes = HashMap::new();
                    let mut headers = BTreeSet::new();
                    headers.insert("#include \"../include/evt.h\"\n".into());
                    headers.insert("#include <stdlib.h>\n".into());
                    let definitions = symdb.rel_iter(area_id)
                        .map(|ent| {
                            gen::generate_line(&overlay, &symdb, ent, &strings)
                                .map(|def| (
                                    def,
                                    &ent.namespace,
                                    sym::SymAddr::Rel(area_id, ent.section_addr())
                                ))
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    for (def, namespace, addr) in definitions {
                        match strings.visited(addr) {
                            None => {}
                            Some(true) => continue,
                            Some(false) => {
                                eprintln!(
                                    "WARNING: {}: string constant is not used",
                                    symdb.symbol_name(addr, false),
                                )
                            }
                        }

                        let mut file = format!("{}.c", area_name);
                        let mut file_include = format!("{}.h", area_name);
                        for ns in namespace.split(" ") {
                            if ns.ends_with(".o") {
                                file = format!("{}.c", &ns[..ns.len() - 2]);
                                file_include =
                                    format!("{}.h", &ns[..ns.len() - 2]);
                                break;
                            }
                        }

                        let code = codes
                            .entry(file)
                            .or_insert_with(BTreeMap::new)
                            .entry(def.order)
                            .or_insert_with(String::new);
                        code.push_str(&def.definition);
                        code.push_str("\n");

                        if let Some(decl) = &def.declare {
                            headers.insert(format!(
                                "#include \"{file_include}\"\n"
                            ));
                            let code = codes
                                .entry(file_include)
                                .or_insert_with(BTreeMap::new)
                                .entry(def.order)
                                .or_insert_with(String::new);
                            code.push_str(decl);
                            code.push_str("\n");
                        }
                    }

                    for (file, code) in codes.into_iter() {
                        let code: String = code.into_values().intersperse("\n".into()).collect();
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
        }
    }
}
