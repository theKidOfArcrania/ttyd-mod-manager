use std::{
    fmt::{self, Write as _},
    marker::PhantomData,
};

use crate::{evt, sym};
use interop::{CDump, CRead, CTypeable, ConstSize, Size};

pub struct Ptr<T: ?Sized> {
    pub addr: sym::SymAddr,
    _phantom: PhantomData<*const T>,
}

impl<T: ?Sized> std::fmt::Debug for Ptr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ptr").field("addr", &self.addr).finish()
    }
}

impl<T: ?Sized> interop::ConstSize for Ptr<T> {
    fn len() -> usize {
        4
    }
}

impl<T: ?Sized> interop::Size for Ptr<T> {
    fn len(&self) -> usize {
        4
    }
}

impl<T: ?Sized> CRead<sym::SymAddr> for Ptr<T> {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: interop::CReader<sym::SymAddr> + ?Sized,
    {
        Ok(Self {
            addr: reader.read_ptr()?,
            _phantom: PhantomData,
        })
    }
}

impl<T: ?Sized> CDump<sym::SymbolDatabase> for Ptr<T> {
    type Error = fmt::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &sym::SymbolDatabase,
    ) -> fmt::Result {
        self.addr.dump(out, ctx)
    }
}

impl<T: CTypeable + ?Sized> CTypeable for Ptr<T> {
    type Canonical = Ptr<T::Canonical>;
    fn compute_type() -> interop::CTypeKind {
        interop::CTypeKind::Ptr(Box::new(interop::CType::new(
            false,
            T::get_type().clone(),
        )))
    }
}

pub struct ConstPtr<T: ?Sized> {
    pub addr: sym::SymAddr,
    _phantom: PhantomData<*const T>,
}

impl<T: ?Sized> std::fmt::Debug for ConstPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ptr").field("addr", &self.addr).finish()
    }
}

impl<T: ?Sized> interop::ConstSize for ConstPtr<T> {
    fn len() -> usize {
        4
    }
}

impl<T: ?Sized> interop::Size for ConstPtr<T> {
    fn len(&self) -> usize {
        4
    }
}

impl<T: ?Sized> CRead<sym::SymAddr> for ConstPtr<T> {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: interop::CReader<sym::SymAddr> + ?Sized,
    {
        Ok(Self {
            addr: reader.read_ptr()?,
            _phantom: PhantomData,
        })
    }
}

impl<T: ?Sized> CDump<sym::SymbolDatabase> for ConstPtr<T> {
    type Error = fmt::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &sym::SymbolDatabase,
    ) -> fmt::Result {
        self.addr.dump(out, ctx)
    }
}

impl<T: CTypeable + ?Sized> CTypeable for ConstPtr<T> {
    type Canonical = ConstPtr<T::Canonical>;
    fn compute_type() -> interop::CTypeKind {
        interop::CTypeKind::Ptr(Box::new(interop::CType::new(
            true,
            T::get_type().clone(),
        )))
    }
}

#[derive(CTypeable, CRead, CDump, Debug, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[repr(u32)]
#[cdump(ctx = sym::SymbolDatabase, error = fmt::Error)]
pub enum NpcTerritoryType {
    NpcTerritoryTypeNothing = 0,
    NpcTerritoryTypeCircle = 1,
    NpcTerritoryTypeSquare = 2,
}

#[derive(Debug, CTypeable, CRead, CDump, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[cdump(ctx = sym::SymbolDatabase, error = fmt::Error)]
pub struct NpcSetupInfo {
    pub name: ConstPtr<str>,
    pub flags: u32,
    pub reaction_flags: u32,
    pub init_event: Ptr<evt::Script>,
    pub regular_event: Ptr<evt::Script>, //0x10, TODO: rename?
    pub talk_event: Ptr<evt::Script>,    //0x14
    pub dead_event: Ptr<evt::Script>,    //0x18
    pub find_event: Ptr<evt::Script>,    //0x1C
    pub lost_event: Ptr<evt::Script>,    //0x20
    pub return_event: Ptr<evt::Script>,  //0x24
    pub blow_event: Ptr<evt::Script>,    //0x28
    pub territory_type: NpcTerritoryType, //0x2C
    pub territory_base: [f32; 3],        //0x30
    pub territory_loiter: [f32; 3],      //0x3C
    pub search_range: f32,               //0x48
    pub search_angle: f32,               //0x4C
    pub homing_range: f32,               //0x50
    pub homing_angle: f32,               //0x54
    pub battle_info_id: u32,             //0x58, TODO: rename?
}
