use crate::{Error, mock::*};
use frame_support::{assert_ok, assert_noop};
use crate::IssueData;

#[test]
fn it_works_for_default_value() {
	new_test_ext().execute_with(|| {
		let stages = IssueModule::stages();
		assert_eq!(stages.len(), mock_stages().len());
		// first should be closed.
		assert!(stages[0].closed);
		assert_eq!(IssueModule::remain(), 50);
		assert_eq!(IssueModule::data(), IssueData { total: 100, unreleased: 50});
	});
}