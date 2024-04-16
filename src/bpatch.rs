use std::{fs::File, io, path::Path};

use error::mk_err_wrapper;

use crate::{dol, rel::BigU32};

#[derive(Debug, thiserror::Error)]
pub enum ErrorType {
    #[error("IO Error: {0}")]
    IOError(io::Error),
    #[error("Bad file magic")]
    BadMagic,
    #[error("Bad file patch type: 0x{0:02x}")]
    BadType(u8),
    #[error("Address unresolvable: 0x{0:08x}")]
    UnknownAddress(u32),
}

mk_err_wrapper! {
    ErrorType {}
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        error!(IOError(value))
    }
}

#[derive(bytemuck::Pod, bytemuck::Zeroable, Clone, Copy, Default, Debug)]
#[repr(C)]
struct PatchFileHeader {
    magic: [u8; 3],
    filetp: u8,
    size: BigU32,
}

#[derive(bytemuck::Pod, bytemuck::Zeroable, Clone, Copy, Default, Debug)]
#[repr(C)]
pub struct DolPatchSegmentHeader {
    pub addr: BigU32,
    pub seg_len: BigU32,
}

pub struct DolPatchSegment {
    pub header: DolPatchSegmentHeader,
    pub content: Vec<u8>,
}

pub struct PatchFile {
    header: PatchFileHeader,
    dol_patches: Vec<DolPatchSegment>,
}

impl PatchFile {
    pub fn from_reader<R: io::Read>(mut rdr: R) -> Res<Self> {
        let mut header = PatchFileHeader::default();
        rdr.read_exact(bytemuck::bytes_of_mut(&mut header))?;

        if &header.magic != b"PAT" {
            bail!(BadMagic);
        }

        if header.filetp != b'D' {
            bail!(BadType(header.filetp));
        }

        let mut dol_patches = Vec::new();
        let mut seghdr = DolPatchSegmentHeader::default();
        for _ in 0..header.size.get() {
            rdr.read_exact(bytemuck::bytes_of_mut(&mut seghdr))?;

            let mut content = vec![0; seghdr.seg_len.get() as usize];
            rdr.read_exact(&mut content)?;

            dol_patches.push(DolPatchSegment {
                header: seghdr,
                content,
            })
        }

        Ok(Self { header, dol_patches })
    }

    pub fn dol_patches<'a>(&'a self) -> core::slice::Iter<'a, DolPatchSegment> {
        self.dol_patches.iter()
    }
}

pub fn patch_dol(
    base: Option<&Path>,
    patch: &Path,
    output: &Path,
) -> anyhow::Result<()> {
    let mut dol = dol::DolFile::from_reader(File::open(
        base.ok_or_else(|| anyhow::anyhow!("Need base dol file to patch from"))?
    )?)?;
    let patches = PatchFile::from_reader(File::open(patch)?)?;
    dol.patch(&patches)?;
    dol.write(File::create(output)?)?;
    Ok(())
}
