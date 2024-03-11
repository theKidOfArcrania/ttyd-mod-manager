use std::{
    collections::BTreeMap,
    error::Error,
    fmt::{self, Display, LowerExp, Write},
};

#[derive(Default)]
pub struct Dumper {
    res: String,
}

impl fmt::Write for Dumper {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.res.push_str(s);
        Ok(())
    }
}

pub trait CDump<Ctx> {
    type Error: Error + From<fmt::Error>;

    fn dump(&self, out: &mut Dumper, ctx: &Ctx) -> Result<(), Self::Error>;
}

macro_rules! impl_CDump {
    ($($tp:ty $(: $dumper:expr)?),* $(,)?) => {
        $(impl_CDump!{{$tp $(: $dumper)?}})*
    };
    ({$tp:ty}) => {
        impl<Ctx> CDump<Ctx> for $tp {
            type Error = fmt::Error;

            fn dump(&self, out: &mut Dumper, _: &Ctx) -> Result<(), Self::Error> {
                write!(out, "{self}")
            }
        }
    };

    ({$tp:ty: $dumper:expr}) => {
        impl<Ctx> CDump<Ctx> for $tp {
            type Error = fmt::Error;

            fn dump(&self, f: &mut Dumper, ctx: &Ctx) -> Result<(), Self::Error> {
                $dumper(self, f, ctx)
            }
        }
    };
}

impl_CDump! {
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32: dump_float,
    f64: dump_float,
}

fn dump_float<T, Ctx>(val: &T, f: &mut Dumper, _: &Ctx) -> fmt::Result where
    T: Display + LowerExp,
{
    let norm = format!("{val}");
    let eform = format!("{val:e}");
    let specials = BTreeMap::from_iter([
        ("NaN", "(0.0/0.0)"),
        ("inf", "(1.0/0.0)"),
        ("-inf", "(-1.0/0.0)"),
    ]);
    if norm.ends_with("000") || norm.starts_with("0.") || norm.starts_with("-0.") {
        write!(f, "{eform}")
    } else if norm.find('.').is_none() {
        write!(f, "{norm}.0")
    } else if let Some(special) = specials.get(norm.as_str()) {
        write!(f, "{special}")
    } else {
        write!(f, "{norm}")
    }
}

fn dump_list<'a, Ctx, T, E>(
    iter: impl Iterator<Item = &'a T>,
    f: &mut Dumper,
    ctx: &Ctx,
) -> Result<(), E> where
    T: CDump<Ctx, Error = E> + 'a,
    E: From<fmt::Error>,
{
    write!(f, "{{")?;
    for (i, elem) in iter.enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        elem.dump(f, ctx)?;
    }
    write!(f, "}}")?;
    Ok(())
}

impl<Ctx, T: CDump<Ctx>> CDump<Ctx> for [T] {
    type Error = T::Error;

    fn dump(&self, f: &mut Dumper, ctx: &Ctx) -> Result<(), Self::Error> {
        dump_list(self.iter(), f, ctx)
    }
}

impl<Ctx, T: CDump<Ctx>, const SIZE: usize> CDump<Ctx> for [T; SIZE] {
    type Error = T::Error;

    fn dump(&self, f: &mut Dumper, ctx: &Ctx) -> Result<(), Self::Error> {
        dump_list(self.iter(), f, ctx)
    }
}

impl<Ctx, T: CDump<Ctx>> CDump<Ctx> for Vec<T> {
    type Error = T::Error;

    fn dump(&self, f: &mut Dumper, ctx: &Ctx) -> Result<(), Self::Error> {
        dump_list(self.iter(), f, ctx)
    }
}

pub fn dumps<Ctx, T: CDump<Ctx> + ?Sized>(val: &T, ctx: &Ctx) -> Result<String, T::Error> {
    let mut dumper = Dumper::default();
    val.dump(&mut dumper, ctx)?;
    Ok(dumper.res)
}
