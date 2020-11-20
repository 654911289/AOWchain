use crate::{mock::*, Error};
use crate::{GovVote, Proposal};
use frame_support::{assert_noop, assert_ok};
use pallet_balances::Error as BalancesError;

fn fast_forward_to(n: u64) {
	Timestamp::set_timestamp(n);
}

#[test]
fn should_open_proposal() {
	new_test_ext().execute_with(|| {
		// check alive
		assert!(GovModule::alive());
		// open next proposal
		assert_ok!(GovModule::open_next(Origin::signed(10)));
		// check step
		assert_eq!(GovModule::current_step(), 0);
		// should in store
		assert_eq!(GovModule::opened_proposal().is_some(), true);
		// can't reopen
		assert_noop!(
			GovModule::open_next(Origin::signed(10)),
			Error::<Test>::UncloseProposal
		);
	});
}

#[test]
fn should_close_proposal() {
	new_test_ext().execute_with(|| {
		assert_noop!(
			GovModule::over(Origin::signed(10)),
			Error::<Test>::UnExistedProposal
		);
		assert_ok!(GovModule::open_next(Origin::signed(10)));
		assert_noop!(
			GovModule::over(Origin::signed(10)),
			Error::<Test>::UnfinishedProposal
		);
		fast_forward_to(5);
		assert_ok!(GovModule::over(Origin::signed(10)));
	});
}

#[test]
fn vote_ok_and_lock_balance() {
	new_test_ext().execute_with(|| {
		assert_ok!(GovModule::open_next(Origin::signed(10)));
		let vt1 = GovVote {
			aye: true,
			balance: 60,
		};
		assert_noop!(
			GovModule::vote(Origin::signed(10), vt1),
			Error::<Test>::InsufficientFunds
		);
		let mut vt2 = GovVote {
			aye: true,
			balance: 40,
		};
		assert_ok!(GovModule::vote(Origin::signed(10), vt2.clone()));
		assert_noop!(
			GovModule::vote(Origin::signed(10), vt2.clone()),
			Error::<Test>::InsufficientFunds
		);
		vt2.balance = 10;
		assert_ok!(GovModule::vote(Origin::signed(10), vt2.clone()));
		// 40 + 10
		vt2.balance = 50;

		assert_eq!(GovModule::votes(1, 10), vt2);
		// check balance, total is 50, lock 50.
		assert_eq!(Balances::free_balance(10), 50);
		assert_noop!(
			Balances::transfer(Origin::signed(10), 20, 1),
			BalancesError::<Test, _>::LiquidityRestrictions
		);

		let vt3 = GovVote {
			aye: false,
			balance: 10,
		};
		assert_ok!(GovModule::vote(Origin::signed(20), vt3));
		let proposal = Proposal {
			step: 1,
			start: GovModule::vote_period(),
			end: GovModule::vote_period(),
			aye: 50,
			nay: 10,
		};
		assert_eq!(GovModule::opened_proposal(), Some(proposal));
	});
}

#[test]
fn close_proposal_and_unlock_balance() {
	new_test_ext().execute_with(|| {
		assert_ok!(GovModule::open_next(Origin::signed(10)));
		// vote aye
		assert_ok!(GovModule::vote(
			Origin::signed(10),
			GovVote {
				aye: false,
				balance: 50
			}
		));
		assert_noop!(
			Balances::transfer(Origin::signed(10), 30, 1),
			BalancesError::<Test, _>::LiquidityRestrictions
		);
		// vote nay
		assert_ok!(GovModule::vote(
			Origin::signed(20),
			GovVote {
				aye: false,
				balance: 50
			}
		));
		assert_noop!(
			Balances::transfer(Origin::signed(10), 30, 1),
			BalancesError::<Test, _>::LiquidityRestrictions
		);

		fast_forward_to(5);

		assert_ok!(GovModule::over(Origin::signed(10)));
		assert_ok!(Balances::transfer(Origin::signed(10), 30, 1));
		assert_ok!(Balances::transfer(Origin::signed(20), 30, 1));
	});
}

#[test]
fn abandoned_proposal_should_broken() {
	new_test_ext().execute_with(|| {
		assert_ok!(GovModule::open_next(Origin::signed(10)));
		assert_ok!(GovModule::vote(
			Origin::signed(10),
			GovVote {
				aye: false,
				balance: 50
			}
		));
		fast_forward_to(5);
		assert_ok!(GovModule::over(Origin::signed(10)));
		// nay state
		assert_eq!(GovModule::current_step(), 0);
		assert!(!GovModule::alive());
		assert_noop!(
			GovModule::open_next(Origin::signed(10)),
			Error::<Test>::BrokenConsensus
		);
	});
}

#[test]
fn pass_proposal_and_can_open_next() {
	new_test_ext().execute_with(|| {
		assert_ok!(GovModule::open_next(Origin::signed(10)));
		assert_ok!(GovModule::vote(
			Origin::signed(10),
			GovVote {
				aye: true,
				balance: 50
			}
		));
		fast_forward_to(5);
		assert_ok!(GovModule::over(Origin::signed(10)));
		assert_eq!(GovModule::current_step(), 1);

		assert_ok!(GovModule::open_next(Origin::signed(10)));
		assert_ok!(GovModule::vote(
			Origin::signed(10),
			GovVote {
				aye: true,
				balance: 50
			}
		));
		fast_forward_to(10);
		assert_ok!(GovModule::over(Origin::signed(10)));
		assert_eq!(GovModule::current_step(), 2);
	});
}

#[test]
fn pass_and_comsue() {
	new_test_ext().execute_with(|| {
		assert_ok!(GovModule::open_next(Origin::signed(10)));
		assert_ok!(GovModule::vote(
			Origin::signed(10),
			GovVote {
				aye: true,
				balance: 50
			}
		));
		fast_forward_to(5);
		assert_ok!(GovModule::over(Origin::signed(10)));
		assert_eq!(GovModule::current_step(), 1);

		assert_eq!(Issue::remain(), 100);
	});
}
