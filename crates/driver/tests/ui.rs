use goldentests::{TestConfig, TestResult};

#[test]
fn goldentests() -> TestResult<()> {
    let config = TestConfig::new("target/debug/crunchc", "examples", ":: ");
    config.run_tests()
}
