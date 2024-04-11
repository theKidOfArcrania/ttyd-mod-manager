use std::{borrow::Cow, cmp::min, collections::BTreeMap, io, mem::size_of};

use bytemuck::{Pod, Zeroable};

use crate::{bpatch, rel::BigU32};

pub const MAX_DOL_SECTIONS: usize = 18;

#[repr(C)]
#[derive(Pod, Zeroable, Default, Clone, Copy, Debug)]
pub struct DolHeader {
    pub offsets: [BigU32; MAX_DOL_SECTIONS],
    pub addrs: [BigU32; MAX_DOL_SECTIONS],
    pub lengths: [BigU32; MAX_DOL_SECTIONS],
    pub bss_addr: BigU32,
    pub bss_length: BigU32,
}

pub struct DolFile<'b> {
    data: Cow<'b, [u8]>,
    header: DolHeader,
    addr_lookup: BTreeMap<u32, DolSection>,
}

#[derive(Clone, Copy)]
pub struct DolSection {
    pub num: usize,
    pub addr: u32,
    pub length: u32,
    pub offset: u32,
}

impl<'b> DolFile<'b> {
    pub fn from_reader<R: io::Read>(mut reader: R) -> io::Result<Self> {
        let mut buf = Vec::new();
        reader.read_to_end(&mut buf)?;
        Ok(Self::new(Cow::Owned(buf)))
    }

    pub fn new_borrowed(data: &'b [u8]) -> Self {
        Self::new(Cow::Borrowed(data))
    }

    fn new(data: Cow<'b, [u8]>) -> Self {
        let header: DolHeader =
            bytemuck::pod_read_unaligned(&data[0..size_of::<DolHeader>()]);
        let mut addr_lookup = BTreeMap::new();
        for (num, addr) in header.addrs.iter().map(BigU32::get).enumerate() {
            if addr != 0 {
                // TODO: ensure that no overlapping sections exist in this
                // lookup
                addr_lookup.insert(addr, DolSection {
                    num,
                    addr,
                    length: header.lengths[num].get(),
                    offset: header.offsets[num].get(),
                });
            }
        }

        // Insert bss into all the gaps not already defined by other sections
        let mut start = header.bss_addr.get();
        let end = start + header.bss_length.get();
        let overlaps: Vec<_> = addr_lookup.range(..start)
            .last()
            .into_iter()
            .chain(addr_lookup.range(start..end))
            .map(|(a, b)| (*a, *b))
            .collect();
        for (_, overlap) in overlaps {
            let overlap_end = overlap.addr + overlap.length;

            // The bss range is already nil
            if end <= start {
                break;
            }

            // Overlap occurs before our bss start, so this has no bearing 
            // on our bss section
            if overlap_end <= start {
                continue;
            }

            // Overlap occurs after start, so we have at least a small segment of
            // bss being created
            if overlap.addr > start {
                addr_lookup.insert(start, DolSection {
                    num: 0,
                    addr: start,
                    length: min(overlap.addr, end) - start,
                    offset: 0,
                });
            }

            // Check again if we have more bss sections after this overlap
            start = overlap_end;
        }

        Self { data, header, addr_lookup }
    }

    pub fn lookup_section(&self, addr: u32) -> Option<&DolSection> {
        self.addr_lookup.range(..=addr).last().map(|(_, a)| a)
    }

    pub fn lookup_section_data(&self, addr: u32) -> Option<Cow<[u8]>> {
        self.addr_lookup.range(..=addr).last().map(|(_, sect)| {
            let offset_in_sect = (addr - sect.addr) as usize;
            if sect.offset == 0 {
                Cow::Owned(vec![0; sect.length as usize - offset_in_sect])
            } else {
                Cow::Borrowed(&self.data.as_ref()[sect.offset as usize..]
                    [offset_in_sect..sect.length as usize])
            }
        })
    }

    pub fn patch(&mut self, patches: &bpatch::PatchFile) -> bpatch::Res<()> {
        #[allow(unused)]
        macro_rules! error {
            ($e:expr) => {{
                use bpatch::ErrorType::*;
                bpatch::Error::new($e)
            }}
        }

        #[allow(unused)]
        macro_rules! bail {
            ($e:expr) => {{
                use bpatch::ErrorType::*;
                return Err(bpatch::Error::new($e));
            }}
        }

        let mut raw_patches = Vec::new();
        for patch in patches.dol_patches() {
            let mut addr = patch.header.addr.get();
            let mut data = patch.content.as_slice();
            while data.len() > 0 {
                let sect = self.lookup_section(addr)
                    .ok_or_else(|| error!(UnknownAddress(addr)))?;
                let sect_offset = addr - sect.addr;
                let sect_size = min(
                    min(data.len(), u32::MAX as usize) as u32,
                    sect.length - sect_offset,
                );
                if sect.offset != 0 {
                    raw_patches.push((
                        (sect.offset + sect_offset) as usize,
                        &data[..sect_size as usize],
                    ));
                }
                addr += sect_size;
                data = &data[sect_size as usize..];
            }
        }

        let data = self.data.to_mut();
        for (offset, new_patch) in raw_patches {
            data[offset..offset+new_patch.len()]
                .copy_from_slice(new_patch)
        }

        Ok(())
    }

    pub fn write<W: io::Write>(&self, mut wtr: W) -> io::Result<()> {
        wtr.write_all(&self.data)
    }
}
