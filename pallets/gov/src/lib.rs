#![cfg_attr(not(feature = "std"), no_std)]

use codec::{Decode, Encode};
use dispatch::DispatchResult;
use frame_support::{
	debug, decl_error, decl_event, decl_module, decl_storage, dispatch, ensure,
	traits::{Currency, Get, LockIdentifier, LockableCurrency, WithdrawReasons},
};
use frame_system::{ensure_signed};
use pallet_issue::Issue;
use sp_runtime::RuntimeDebug;
use sp_std::prelude::*;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

const GOV_ID: LockIdentifier = *b"issuegov";

pub trait Trait: pallet_timestamp::Trait {
	/// Because this pallet emits events, it depends on the runtime's definition of an event.
	type Event: From<Event<Self>> + Into<<Self as frame_system::Trait>::Event>;

	type Currency: LockableCurrency<Self::AccountId>;

	type Issue: Issue<BalanceOf<Self>>;

	type VoteBlockTime: Get<Self::Moment>;
}

#[derive(Default, PartialEq, Eq, Clone, Encode, Decode, RuntimeDebug)]
pub struct Proposal<Moment, Balance> {
	step: u32,
	start: Moment,
	end: Moment,
	aye: Balance,
	nay: Balance,
}

#[derive(Default, PartialEq, Eq, Clone, Encode, Decode, RuntimeDebug)]
pub struct GovVote<Balance> {
	pub aye: bool,
	pub balance: Balance,
}

pub type BalanceOf<T> =
	<<T as Trait>::Currency as Currency<<T as frame_system::Trait>::AccountId>>::Balance;
pub type MomentOf<T> = <T as pallet_timestamp::Trait>::Moment;
pub type ProposalT<T> = Proposal<MomentOf<T>, BalanceOf<T>>;

decl_storage! {
	trait Store for Module<T: Trait> as Gov {
		VotePeriod get(fn vote_period) config() : MomentOf<T>;
		Alive get(fn alive): bool = true;
		CurrentStep get(fn current_step): u32 = 0;
		OpenedProposal get(fn opened_proposal):  Option<ProposalT<T>>;
		HistoryProposals get(fn history_proposals): Vec<ProposalT<T>>;
		Votes get(fn votes): double_map hasher(twox_64_concat) u32, hasher(twox_64_concat) T::AccountId => GovVote<BalanceOf<T>>;
		VotingOf get(fn voting_of): map hasher(twox_64_concat) u32 => Vec<T::AccountId>;
	}
}

decl_event!(
	pub enum Event<T>
	where
		AccountId = <T as frame_system::Trait>::AccountId,
		Balance = BalanceOf<T>,
	{
		OpenProposal(u32),                    // step
		CloseProposal(u32, bool),             // step, state
		Voted(u32, AccountId, bool, Balance), // step, account, t/f,  mount
	}
);

decl_error! {
	pub enum Error for Module<T: Trait> {
		BrokenConsensus,
		UncloseProposal,
		UnExistedProposal,
		UnfinishedProposal,
		InsufficientFunds,
		AlreadyVetoed,
		OutdatedProposal,
		VoteDiffCamps,
		NotOperateAccount,
	}
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		// Errors must be initialized if they are used by the pallet.
		type Error = Error<T>;

		// Events must be initialized if they are used by the pallet.
		fn deposit_event() = default;

		#[weight = 0]
		pub fn reset_vote_period(origin, period: MomentOf<T>) -> DispatchResult {
			let who = ensure_signed(origin)?;

			//verify operate account
			ensure!(Self::verify_operate_account(who), Error::<T>::NotOperateAccount);
			
			<VotePeriod<T>>::set(period);
			Ok(())
		}

		#[weight = 0]
		pub fn open_next(origin) -> DispatchResult {
			let who = ensure_signed(origin)?;

			//verify operate account
			ensure!(Self::verify_operate_account(who), Error::<T>::NotOperateAccount);

			ensure!(Alive::get(), Error::<T>::BrokenConsensus);
			ensure!(<OpenedProposal<T>>::get().is_none(), Error::<T>::UncloseProposal);

			let next_step = CurrentStep::get() + 1;
			let start_time = <pallet_timestamp::Module<T>>::get();
			let end_time = <pallet_timestamp::Module<T>>::get() + <VotePeriod<T>>::get();
			let proposal = Proposal {
				step: next_step,
				start: start_time,
				end: end_time,
				..Default::default()
			};

			<OpenedProposal<T>>::set(Some(proposal));
			Self::deposit_event(RawEvent::OpenProposal(next_step));
			Ok(())
		}

		#[weight = 0]
		pub fn vote(origin, vote: GovVote<BalanceOf<T>>) -> DispatchResult {
			let who = ensure_signed(origin)?;

			ensure!(Alive::get(), Error::<T>::BrokenConsensus);
			let mut prop = <OpenedProposal<T>>::get().ok_or(Error::<T>::UnExistedProposal)?;
			ensure!(!Self::is_finish(prop.end), Error::<T>::OutdatedProposal);

			if <Votes<T>>::contains_key(&prop.step, &who) {
				let mut s_vote = <Votes<T>>::get(&prop.step, &who);
				ensure!(s_vote.aye == vote.aye, Error::<T>::VoteDiffCamps);
				s_vote.balance += vote.balance;
				ensure!(s_vote.balance <= T::Currency::free_balance(&who), Error::<T>::InsufficientFunds);
				<Votes<T>>::insert(prop.step, &who, &s_vote);

				T::Currency::extend_lock(
					GOV_ID,
					&who,
					s_vote.balance,
					WithdrawReasons::all()
				);
			} else {
				ensure!(vote.balance <= T::Currency::free_balance(&who), Error::<T>::InsufficientFunds);
				<Votes<T>>::insert(prop.step, &who, &vote);
				<VotingOf<T>>::mutate(prop.step, |person| person.push(who.clone()));

				T::Currency::set_lock(
					GOV_ID,
					&who,
					vote.balance,
					WithdrawReasons::all()
				);
			}

			// change total.
			if vote.aye {
				prop.aye += vote.balance;
			} else {
				prop.nay += vote.balance;
			}

			// change storage
			<OpenedProposal<T>>::put(&prop);

			Self::deposit_event(RawEvent::Voted(prop.step, who, vote.aye, vote.balance));
			Ok(())
		}

		#[weight = 0]
		pub fn over(origin) -> DispatchResult {
			let _sender = ensure_signed(origin)?;
			//verify operate account
			ensure!(Self::verify_operate_account(_sender), Error::<T>::NotOperateAccount);

			ensure!(Alive::get(), Error::<T>::BrokenConsensus);
			let prop = <OpenedProposal<T>>::get().ok_or(Error::<T>::UnExistedProposal)?;
			ensure!(Self::is_finish(prop.end), Error::<T>::UnfinishedProposal);

			let aye = Self::judge(&prop);
			if aye {
				Self::inject();
				CurrentStep::set(prop.step);
			} else {
				// broken
				Alive::put(false);
			}
			// unlock vote.
			for person in <VotingOf<T>>::get(prop.step) {
				T::Currency::remove_lock(
					GOV_ID,
					&person,
				);
			}

			<OpenedProposal<T>>::set(None);
			<HistoryProposals<T>>::mutate(|props| props.push(prop.clone()));
			Self::deposit_event(RawEvent::CloseProposal(prop.step, aye));
			Ok(())
		}

	}
}

impl<T: Trait> Module<T> {
	fn inject() {
		// update issue stage.
		T::Issue::consume();
	}

	fn is_finish(end: MomentOf<T>) -> bool {
		let now = <pallet_timestamp::Module<T>>::get();
		end < now
	}

	fn judge(props: &ProposalT<T>) -> bool {
		props.aye > props.nay
	}

	fn verify_operate_account(sender: T::AccountId) -> bool {
		//get operate account from pallet issue
		let account_vu8 = T::Issue::get_operate_account();
		let account_u8 = account_vu8.as_slice();
		let decoded_ss58 = bs58::decode(account_u8).into_vec().unwrap();
		let mut data = [0u8; 32];
		if decoded_ss58.len() == 35 {
			data.copy_from_slice(&decoded_ss58[1..33]);
		}
		let op_account = T::AccountId::decode(&mut &data[..]).unwrap();

		//compare sender&op_account
		if sender == op_account {
			true
		} else {
			false
		}
	}
}
