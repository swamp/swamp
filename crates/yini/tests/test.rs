use swamp_yini::read_yini_from_str;

#[test]
pub fn members() {
    let ini = read_yini_from_str(r#"members ["test/", another/sub_dir] "#).unwrap();
    assert_eq!(ini.members.len(), 2, "should be 2");
    assert_eq!(ini.members[0].to_str().unwrap(), "test/");
    assert_eq!(ini.members[1].to_str().unwrap(), "another/sub_dir");
}
