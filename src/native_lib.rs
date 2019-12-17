#![allow(dead_code)]

use crate::{Result, RuntimeError, RuntimeErrorTy, RuntimeValue};
use libloading::{Library, Symbol};
use std::collections::HashMap;

const FFI_ENTRY_POINT_PREFIX: &'static [u8] = b"___crunch_native_";
type ExternalFuncSignature = extern "C" fn(input: Vec<RuntimeValue>) -> Vec<RuntimeValue>;

struct FFIHandler {
    libs: HashMap<usize, Library>,
}

impl FFIHandler {
    pub fn load(&mut self, name: impl AsRef<std::ffi::OsStr>, id: usize) -> Result<()> {
        let name = name.as_ref();

        let lib = match Library::new(name) {
            Ok(lib) => lib,
            Err(err) => {
                error!("Dynamic Lib not found: {:?}", err);

                return Err(RuntimeError {
                    ty: RuntimeErrorTy::MissingFile,
                    message: format!("The dynamic library {:?} cannot be found", name),
                });
            }
        };

        self.libs.insert(id, lib);

        Ok(())
    }

    pub fn get_func<'a>(
        &'a self,
        func_name: impl AsRef<[u8]>,
        lib: usize,
    ) -> Result<Symbol<'a, ExternalFuncSignature>> {
        let func_name = func_name.as_ref();

        let lib = if let Some(lib) = self.libs.get(&lib) {
            lib
        } else {
            error!(
                "Error getting lib id {}, attempting to use function {:?}",
                lib, func_name
            );

            return Err(RuntimeError {
                ty: RuntimeErrorTy::MissingFile,
                message: "The requested Dynamic Library is not loaded".to_string(),
            });
        };

        let func = match unsafe { lib.get(func_name) } {
            Ok(func) => func,
            Err(err) => {
                error!(
                    "A function by the name of {:?} cannot be found: {:?}",
                    func_name, err
                );

                return Err(RuntimeError {
                    ty: RuntimeErrorTy::MissingFile, // MissingFunction?
                    message: "The requested Dynamic Library does not have the requested function"
                        .to_string(),
                });
            }
        };

        Ok(func)
    }
}
