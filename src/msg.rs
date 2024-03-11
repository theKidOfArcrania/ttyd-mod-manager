use std::collections::HashMap;

use anyhow::{anyhow, bail};

#[derive(Clone, Default)]
pub struct Messages {
    msg_order: Vec<Vec<u8>>,
    msg_map: HashMap<Vec<u8>, Vec<u8>>,
}

impl Messages {
    pub fn parse(msg: Vec<u8>) -> anyhow::Result<Self> {
        let mut ret = Self::default();
        let mut parts = msg.split(|b| b == &0);
        loop {
            let msgkey = parts
                .next()
                .ok_or_else(|| anyhow!("EOF while reading message file"))?;
            if msgkey.is_empty() {
                break;
            }

            let msgvalue = parts
                .next()
                .ok_or_else(|| anyhow!("EOF while reading msg base"))?;
            if !ret.add_message(msgkey.to_vec(), msgvalue.to_vec()) {
                bail!(
                    "Two message keys with value {}",
                    String::from_utf8_lossy(msgkey),
                );
            }
        }

        Ok(ret)
    }

    pub fn patch_with(&mut self, patch: Vec<u8>) {
        let mut msgkey: Option<&[u8]> = None;
        let mut msgvalue = Vec::new();
        for line in patch.trim_ascii_end().split(|b| b == &0xa) {
            if let Some(key) =
                line.strip_prefix(b"[#").and_then(|l| l.strip_suffix(b"#]"))
            {
                if let Some(oldkey) = msgkey {
                    self.add_message(oldkey.to_vec(), msgvalue);
                    msgvalue = Vec::new();
                }
                msgkey = Some(key.trim_ascii());
            } else {
                if !msgvalue.is_empty() {
                    msgvalue.push(b'\n');
                }
                msgvalue.extend_from_slice(line);
            }
        }

        if let Some(oldkey) = msgkey {
            self.add_message(oldkey.to_vec(), msgvalue);
        }
    }

    pub fn add_message(&mut self, key: Vec<u8>, value: Vec<u8>) -> bool {
        if self.msg_map.insert(key.clone(), value).is_none() {
            self.msg_order.push(key);
            true
        } else {
            false
        }
    }

    pub fn to_patchfile(mut self) -> Vec<u8> {
        let mut ret = Vec::new();

        // TODO: make sure that all messages won't generate an invalid patch
        // file format
        for msg in self.msg_order {
            ret.extend_from_slice(b"[#");
            ret.extend_from_slice(&msg);
            ret.extend_from_slice(b"#]\n");

            let body = self
                .msg_map
                .remove(&msg)
                .expect("Should be valid message key");
            ret.extend_from_slice(&body);
            ret.push(b'\n');
        }

        ret
    }

    pub fn to_message(mut self) -> Vec<u8> {
        let mut ret = Vec::new();
        for msg in self.msg_order {
            ret.extend_from_slice(&msg);
            ret.push(0);

            let body = self
                .msg_map
                .remove(&msg)
                .expect("Should be valid message key");
            ret.extend_from_slice(&body);
            ret.push(0);
        }
        ret.push(0);
        ret
    }
}

pub fn patch_msgfile(
    base: Option<Vec<u8>>,
    patch: Vec<u8>,
) -> anyhow::Result<Vec<u8>> {
    let mut msgs = match base {
        None => bail!("Missing base file for patch"),
        Some(base) => Messages::parse(base)?,
    };

    msgs.patch_with(patch);
    Ok(msgs.to_message())
}
