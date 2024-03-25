use std::{
    fmt::{self, Write as _},
    marker::PhantomData,
};

use crate::{evt, reader, sym};
use interop::{CDump, CRead, CTypeable, ConstSize, Size};
use serde::{Serialize, Deserialize};

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

impl<T: ?Sized> CDump<sym::AddrDumpCtx<'_>> for Ptr<T> {
    type Error = fmt::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &sym::AddrDumpCtx,
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

impl<T: ?Sized> CDump<sym::AddrDumpCtx<'_>> for ConstPtr<T> {
    type Error = fmt::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &sym::AddrDumpCtx,
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
#[cdump(ctx = sym::AddrDumpCtx<'_>, error = fmt::Error)]
pub enum BattleUnitType {
	KNullUnitKind, //0x0
	KUnitGoomba, //0x1
	KUnitParagoomba, //0x2
	KUnitSpikyGoomba, //0x3
	KUnitSpinia, //0x4
	KUnitSpania, //0x5
	KUnitLordCrumpPrologue, //0x6
	KUnitGus, //0x7
	KUnitBlooper, //0x8
	KUnitBlooperLeftTentacle, //0x9
	KUnitBlooperRightTentacle, //0xA
	KUnitKoopatrol, //0xB
	KUnitMagikoopa, //0xC
	KUnitMagikoopaClone, //0xD
	KUnitKoopaTroopa, //0xE
	KUnitParatroopa, //0xF
	KUnitFuzzy, //0x10
	KUnitDullBones, //0x11
	KUnitBaldCleft, //0x12
	KUnitBristle, //0x13
	KUnitGoldFuzzy, //0x14
	KUnitFuzzyHorde, //0x15
	KUnitRedBones, //0x16
	KUnitHooktail, //0x17
	KUnitDarkPuff, //0x18
	KUnitPalePiranha, //0x19
	KUnitCleft, //0x1A
	KUnitPider, //0x1B
	KUnitXNaut, //0x1C
	KUnitYux, //0x1D
	KUnitMiniYux, //0x1E
	KUnitBeldamChapter2, //0x1F
	KUnitMarilynChapter2, //0x20
	KUnitVivianChapter2, //0x21
	KUnitMagnus, //0x22
	KUnitXFist, //0x23
	KUnitGoombaGlitzville, //0x24
	KUnitKpKoopa, //0x25
	KUnitKpParatroopa, //0x26
	KUnitPokey, //0x27
	KUnitLakitu, //0x28
	KUnitSpiny, //0x29
	KUnitHyperBaldCleft, //0x2A
	KUnitBobOmb, //0x2B
	KUnitBandit, //0x2C
	KUnitBigBandit, //0x2D
	KUnitRedSpikyBuzzy, //0x2E
	KUnitShadyKoopa, //0x2F
	KUnitShadyParatroopa, //0x30
	KUnitRedMagikoopa, //0x31
	KUnitRedMagikoopaClone, //0x32
	KUnitWhiteMagikoopa, //0x33
	KUnitWhiteMagikoopaClone, //0x34
	KUnitGreenMagikoopa, //0x35
	KUnitGreenMagikoopaClone, //0x36
	KUnitDarkCraw, //0x37
	KUnitHammerBro, //0x38
	KUnitBoomerangBro, //0x39
	KUnitFireBro, //0x3A
	KUnitRedChomp, //0x3B
	KUnitDarkKoopatrol, //0x3C
	KUnitIronCleftRed, //0x3D
	KUnitIronCleftGreen, //0x3E
	KUnitBowserChapter3, //0x3F
	KUnitRawkHawk, //0x40
	KUnitMachoGrubba, //0x41
	KUnitHyperGoomba, //0x42
	KUnitHyperParagoomba, //0x43
	KUnitHyperSpikyGoomba, //0x44
	KUnitCrazeeDayzee, //0x45
	KUnitAmazyDayzee, //0x46
	KUnitHyperCleft, //0x47
	KUnitBuzzyBeetle, //0x48
	KUnitSpikeTop, //0x49
	KUnitSwooper, //0x4A
	KUnitBoo, //0x4B
	KUnitAtomicBoo, //0x4C
	KUnitDooplissChapter4Fight1, //0x4D
	KUnitDooplissChapter4Invincible, //0x4E
	KUnitDooplissChapter4Fight2, //0x4F
	KUnitGoombellaChapter4, //0x50
	KUnitKoopsChapter4, //0x51
	KUnitYoshiChapter4, //0x52
	KUnitFlurrieChapter4, //0x53
	KUnitEmber, //0x54
	KUnitLavaBubble, //0x55
	KUnitGreenFuzzy, //0x56
	KUnitFlowerFuzzy, //0x57
	KUnitPutridPiranha, //0x58
	KUnitParabuzzy, //0x59
	KUnitBillBlaster, //0x5A
	KUnitBulletBill, //0x5B
	KUnitBulkyBobOmb, //0x5C
	KUnitCortez, //0x5D
	KUnitCortezBonePile, //0x5E
	KUnitCortezSword, //0x5F
	KUnitCortezHook, //0x60
	KUnitCortezRapier, //0x61
	KUnitCortezSaber, //0x62
	KUnitLordCrumpChapter5, //0x63
	KUnitXNautsCrumpFormation1, //0x64
	KUnitXNautsCrumpFormation2, //0x65
	KUnitXNautsCrumpFormation3, //0x66
	KUnitRuffPuff, //0x67
	KUnitPoisonPokey, //0x68
	KUnitSpikyParabuzzy, //0x69
	KUnitDarkBoo, //0x6A
	KUnitSmorg, //0x6B
	KUnitSmorgMiasmaTentacleA, //0x6C
	KUnitSmorgMiasmaTentacleB, //0x6D
	KUnitSmorgMiasmaTentacleC, //0x6E
	KUnitSmorgMiasmaClaw, //0x6F
	KUnitIcePuff, //0x70
	KUnitFrostPiranha, //0x71
	KUnitMoonCleft, //0x72
	KUnitZYux, //0x73
	KUnitMiniZYux, //0x74
	KUnitXYux, //0x75
	KUnitMIniXYux, //0x76
	KUnitXNautPhD, //0x77
	KUnitEliteXNaut, //0x78
	KUnitMagnus20, //0x79
	KUnitXPunch, //0x7A
	KUnitSwoopula, //0x7B
	KUnitPhantomEmber, //0x7C
	KUnitBombshellBillBlaster, //0x7D
	KUnitBombshellBill, //0x7E
	KUnitChainChomp, //0x7F
	KUnitDarkWizzerd, //0x80
	KUnitDarkWizzerdClone, //0x81
	KUnitDryBones, //0x82
	KUnitDarkBones, //0x83
	KUnitGloomtail, //0x84
	KUnitBeldamChapter8, //0x85
	KUnitMarilynChapter8, //0x86
	KUnitDooplissChapter8, //0x87
	KUnitDooplissChapter8Mario, //0x88
	KUnitDooplissChapter8Goombella, //0x89
	KUnitDooplissChapter8Koops, //0x8A
	KUnitDooplissChapter8Yoshi, //0x8B
	KUnitDooplissChapter8Flurrie, //0x8C
	KUnitDooplissChapter8Vivian, //0x8D
	KUnitDooplissChapter8Bobbery, //0x8E
	KUnitDooplissChapter8MsMowz, //0x8F
	KUnitBowserChapter8, //0x90
	KUnitKammyKoopa, //0x91
	KUnitGrodus, //0x92
	KUnitGrodusX, //0x93
	KUnitShadowQueenPeach, //0x94
	KUnitShadowQueenInvincible, //0x95
	KUnitShadowQueenDemon, //0x96
	KUnitShadowQueenLeftRightHand, //0x97
	KUnitShadowQueenDeadHands, //0x98
	KUnitGloomba, //0x99
	KUnitParagloomba, //0x9A
	KUnitSpikyGloomba, //0x9B
	KUnitDarkKoopa, //0x9C
	KUnitDarkParatroopa, //0x9D
	KUnitBadgeBandit, //0x9E
	KUnitDarkLakitu, //0x9F
	KUnitSkyBlueSpiny, //0xA0
	KUnitWizzerd, //0xA1
	KUnitPiranhaPlant, //0xA2
	KUnitSpunia, //0xA3
	KUnitArantula, //0xA4
	KUnitDarkBristle, //0xA5
	KUnitPoisonPuff, //0xA6
	KUnitSwampire, //0xA7
	KUnitBobUlk, //0xA8
	KUnitEliteWizzerd, //0xA9
	KUnitEliteWizzerdClone, //0xAA
	KUnitBonetail, //0xAB
	KUnitUnusedRedBuzzy, //0xAC
	KUnitUnusedRedParabuzzy, //0xAD
	KUnitUnusedRedSpikyParabuzzy, //0xAE
	KUnitUnusedHyperBobOmb, //0xAF
	KUnitUnusedUltraBobOmb, //0xB0
	KUnitTutorialGoombella, //0xB1
	KUnitTutorialFranklyB2, //0xB2
	KUnitTutorialFranklyB3, //0xB3
	KUnitTutorialFranklyB4, //0xB4
	KUnitEpilogueDooplissMario, //0xB5
	KUnitEpilogueFlurrie, //0xB6
	KUnitEpilogueBoo, //0xB7
	KUnitEpilogueAtomicBoo, //0xB8
	KUnitEpilogueMaleToad, //0xB9
	KUnitEpilogueFemaleToad, //0xBA
	KUnitUnusedTest, //0xBB
	KUnitUnusedCrump2, //0xBC
	KUnitUnusedBeldam2, //0xBD
	KUnitUnusedMarilyn2, //0xBE
	KUnitUnusedVivian2, //0xBF
	KUnitUnusedBeldam3, //0xC0
	KUnitUnusedMarilyn3, //0xC1
	KUnitUnusedMechaKuri, //0xC2
	KUnitUnusedMechaKame, //0xC3
	KUnitUnusedOkorl, //0xC4
	KUnitUnusedYowarl, //0xC5
	KUnitUnusedTuyonarl, //0xC6
	KUnitUnusedWanawana, //0xC7
	KUnitUnusedMinaraiKamec, //0xC8
	KUnitUnusedShyGuy, //0xC9
	KUnitUnusedGrooveGuy, //0xCA
	KUnitUnusedPyroGuy, //0xCB
	KUnitUnusedSpyGuy, //0xCC
	KUnitUnusedAntiGuy, //0xCD
	KUnitUnusedBzzap, //0xCE
	KUnitUnusedMiniBzzap, //0xCF
	KUnitUnusedUfo, //0xD0
	KUnitUnusedPennington, //0xD1
	KUnitUnusedFighter, //0xD2
	KUnitUnusedZessT, //0xD3
	KUnitUnusedMaster, //0xD4
	KUnitUnusedReporter, //0xD5
	KUnitUnusedHotdogMaster, //0xD6
	KUnitUnusedFlavio, //0xD7
	KUnitUnusedTree, //0xD8
	KUnitUnusedSwitch, //0xD9
	KUnitUnusedTestnpc, //0xDA
	KUnitBombSquadBomb, //0xDB
	KUnitSystem, //0xDC
	KUnitPrologueGoombella, //0xDD
	KUnitMario, //0xDE
	KUnitShellShield, //0xDF
	KUnitGoombella, //0xE0
//#define TYPE_PARTNER_MIN kUnitGoombella
	KUnitKoops, //0xE1
	KUnitYoshi, //0xE2
	KUnitFlurrie, //0xE3
	KUnitVivian, //0xE4
	KUnitBobbery, //0xE5
	KUnitMsMowz, //0xE6
//#define TYPE_PARTNER_MAX kUnitMsMowz+1
	KUnitMax //0xE7
}

#[derive(CTypeable, CRead, CDump, Debug, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[cdump(ctx = sym::AddrDumpCtx<'_>, error = fmt::Error)]
#[repr(C)]
struct BattleDataEntry {
    tag: u32,
    data: Ptr<[u8]>, // TODO: void*
}

#[derive(CTypeable, CRead, CDump, Debug, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[cdump(ctx = sym::AddrDumpCtx<'_>, error = fmt::Error)]
#[repr(C)]
pub struct BattleUnitDefense {
    defenses: [u8; 5]
}

#[derive(CTypeable, CRead, CDump, Debug, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[cdump(ctx = sym::AddrDumpCtx<'_>, error = fmt::Error)]
#[repr(C)]
pub struct BattleUnitDefenseAttr {
    defense_attrs: [u8; 5]
}

#[derive(CTypeable, CRead, CDump, Debug, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[cdump(ctx = sym::AddrDumpCtx<'_>, error = fmt::Error)]
#[repr(C)]
pub struct BattleUnitKind {
    m_unit_type: BattleUnitType, //0x0
    field_0x4: [u8; 0xC - 0x4], //0x4
    m_danger_hp: u8, //0xC
    m_peril_hp: u8, //0xD
    m_level: u8, //0xE
    m_bonus_exp: u8, //0xF
    field_0x10: [u8; 0xB8 - 0x10], //0x10
    m_parts: Ptr<[u8]>, //0xB8 // TODO BattleWorkUnitPart
    field_0xbc: [u8; 4], //0xBC
    data_table: Ptr<BattleDataEntry>, //0xC0
}

#[derive(Debug, CTypeable, CRead, CDump, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[cdump(ctx = sym::AddrDumpCtx<'_>, error = fmt::Error)]
#[repr(C)]
pub struct BeroEntry {
	hit_name: ConstPtr<str>, //0x0
	kinddir_arg1: u16, //0x4
	kinddir_arg2: u16, //0x6
	kinddir_arg3: u32, //0x8
	field_0xc: u32, //0xC
	unknown1: u32, //0x10
	unknown2: u32, //0x14
	field_0x18: u32, //0x18
	enter_event: Ptr<evt::Script>, //0x1C,
	field_0x20: u32, //0x20
	exit_event: Ptr<evt::Script>, //0x24
	nextarea_arg1: u32, //0x28
	nextarea_arg2: ConstPtr<str>, //0x2C
	anime_arg1: u16, //0x30
	anime_arg2: u16, //0x32
	anime_arg3: u32, //0x34
	anime_arg4: u32, //0x38
}

#[derive(CTypeable, CRead, CDump, Debug, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[repr(u32)]
#[cdump(ctx = sym::AddrDumpCtx<'_>, error = fmt::Error)]
pub enum NpcTerritoryType {
    NpcTerritoryTypeNothing = 0,
    NpcTerritoryTypeCircle = 1,
    NpcTerritoryTypeSquare = 2,
}

#[derive(Debug, CTypeable, CRead, CDump, Size, ConstSize)]
#[cread(ptr = sym::SymAddr)]
#[cdump(ctx = sym::AddrDumpCtx<'_>, error = fmt::Error)]
#[repr(C)]
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

#[derive(Debug, CTypeable, Size, ConstSize)]
#[allow(non_camel_case_types)]
struct stub(u32);

impl<Ctx> interop::CDump<Ctx> for stub {
    type Error = std::fmt::Error;

    fn dump(&self, _out: &mut interop::Dumper, _ctx: &Ctx) -> Result<(), Self::Error> {
        Err(std::fmt::Error)
    }
}

impl<P: interop::Ptr> interop::CRead<P> for stub {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error> where
        R: interop::CReader<P> + ?Sized
    {
        Err(reader.error_custom("Not implemented!".into()))
    }
}

macro_rules! mk_clsdata {
    ($($tp:ident: $tparr: ident $(!$stub:ident)?),*$(,)?) => {
        #[derive(Debug)]
        pub enum ClsData {
            $($tp($tp)),*,
            $($tparr(Vec<$tp>)),*
        }

        #[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Debug)]
        pub enum ClsDataType {
            $($tp),*,
            $($tparr),*
        }

        $($(
            #[derive(Debug, CTypeable, Size, ConstSize)]
            #[repr(C)]
            pub struct $tp($stub);

            impl<Ctx> interop::CDump<Ctx> for $tp {
                type Error = std::fmt::Error;

                fn dump(&self, _out: &mut interop::Dumper, _ctx: &Ctx) -> Result<(), Self::Error> {
                    Err(std::fmt::Error)
                }
            }

            impl<P: interop::Ptr> interop::CRead<P> for $tp {
                fn read<R>(reader: &mut R) -> Result<Self, R::Error> where
                    R: interop::CReader<P> + ?Sized
                {
                    Err(reader.error_custom("Not implemented!".into()))
                }
            }
        )?)*

        impl ClsDataType {
            pub fn read(
                self,
                reader: &mut reader::Reader,
            ) -> Result<ClsData, reader::Error>
            {
                use interop::CReader;
                Ok(match self {
                    $(ClsDataType::$tp => ClsData::$tp(reader.read_val_full()?)),*,
                    // TODO: return arr type
                    $(ClsDataType::$tparr => ClsData::$tparr(reader.read_val_full()?)),*,
                })
            }
        }

        impl ClsData {
            pub fn get_type(&self) -> interop::CTypeKind {
                match self {
                    $(ClsData::$tp(_) => <$tp>::get_type()),*,
                    $(ClsData::$tparr(_) => <$tp>::get_type()),*,
                }
            }
        }

        impl interop::CDump<sym::AddrDumpCtx<'_>> for ClsData {
            type Error = fmt::Error;

            fn dump(
                &self,
                out: &mut interop::Dumper,
                ctx: &sym::AddrDumpCtx,
            ) -> Result<(), Self::Error> {
                match self {
                    $(ClsData::$tp(dt) => dt.dump(out, ctx)),*,
                    $(ClsData::$tparr(dt) => dt.dump(out, ctx)),*,
                }
            }
        }

        impl interop::Size for ClsData {
            fn len(&self) -> usize {
                match self {
                    $(ClsData::$tp(dt) => interop::Size::len(dt)),*,
                    $(ClsData::$tparr(dt) => interop::Size::len(dt)),*,
                }
            }
        }
    }
}


mk_clsdata! {
    AudienceItemWeight: AudienceItemWeightArr !stub,
    BattleGroupSetup: BattleGroupSetupArr !stub,
    BattleSetupData: BattleSetupDataArr !stub,
    BattleSetupNoTable: BattleSetupNoTableArr !stub,
    BattleSetupWeightedLoadout: BattleSetupWeightedLoadoutArr !stub,
    BattleStageFallObjectData: BattleStageFallObjectDataArr !stub,
    BattleStageNozzleData: BattleStageNozzleDataArr !stub,
    BattleStageObjectData: BattleStageObjectDataArr !stub,
    BattleStageData: BattleStageDataArr !stub,
    BattleUnitKind: BattleUnitKindArr,
    BattleUnitKindPart: BattleUnitKindPartArr !stub,
    BattleUnitDataTable: BattleUnitDataTableArr !stub,
    BattleUnitDefense: BattleUnitDefenseArr,
    BattleUnitDefenseAttr: BattleUnitDefenseAttrArr,
    BattleUnitPoseTable: BattleUnitPoseTableArr !stub,
    BattleUnitSetup: BattleUnitSetupArr !stub,
    BattleWeapon: BattleWeaponArr !stub,
    BeroEntry: BeroEntryArr,
    CookingRecipe: CookingRecipeArr !stub,
    ItemData: ItemDataArr !stub,
    ItemDropData: ItemDropDataArr !stub,
    NpcAiTypeTable: NpcAiTypeTableArr !stub,
    NpcSetupInfo: NpcSetupInfoArr,
    PointDropData: PointDropDataArr !stub,
    ShopItemTable: ShopItemTableArr !stub,
    ShopSellPriceList: ShopSellPriceListArr !stub,
    StatusVulnerability: StatusVulnerabilityArr !stub,
}
