pub mod prelude;

use std::fs;
use std::path::{Path, PathBuf};
use tracing::debug;

pub enum ProjectType {
    Library,
    Executable,
}

pub struct SwampIni {
    pub members: Vec<PathBuf>,
    pub ty: ProjectType,
}

impl Default for SwampIni {
    fn default() -> Self {
        Self::new()
    }
}

impl SwampIni {
    #[must_use]
    pub const fn new() -> Self {
        Self { members: vec![], ty: ProjectType::Executable }
    }
}

#[must_use]
pub fn read_yini_cwd() -> Option<SwampIni> {
    read_yini(Path::new("./"))
}

#[must_use]
pub fn read_yini_cwd_with_defaults() -> SwampIni {
    read_yini_with_defaults(Path::new("./"))
}

pub fn read_yini(path: &Path) -> Option<SwampIni> {
    let complete_path = path.join("swamp.yini");
    if let Ok(ini_content) = fs::read_to_string(&complete_path) {
        debug!(?complete_path, "found path");
        read_yini_from_str(&ini_content)
    } else {
        debug!(?path, "could not find path");
        None
    }
}

#[must_use]
pub fn read_yini_with_defaults(path: &Path) -> SwampIni {
    if let Some(found) = read_yini(path) {
        found
    } else {
        SwampIni {
            members: vec![Path::new("scripts/").to_path_buf()],
            ty: ProjectType::Library,
        }
    }
}

pub fn read_yini_from_str(ini_content: &str) -> Option<SwampIni> {
    let parser = yini::Parser::new(ini_content).parse();
    let mut ini = SwampIni::new();

    if let Some(members) = parser.get("members").and_then(|x| x.as_array()) {
        debug!("found members");

        let mut vec = Vec::new();

        for member in members {
            vec.push(Path::new(member.as_str().unwrap()).to_path_buf());
        }

        ini.members = vec;
    }

    if let Some(project_type) = parser.get("type").and_then(|x| x.as_str()) {
        ini.ty = if project_type == "lib" { ProjectType::Library } else { ProjectType::Executable };
    }

    Some(ini)
}
