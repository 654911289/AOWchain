#![cfg_attr(not(feature = "std"), no_std)]

use sp_std::prelude::*;
use frame_support::{ensure, decl_module, decl_storage, decl_event, decl_error, dispatch,
					traits::{Get, Currency, LockableCurrency, LockIdentifier, WithdrawReasons}};
use frame_system::ensure_signed;
use dispatch::DispatchResult;
use codec::{Decode, Encode};
use sp_runtime::{
	RuntimeDebug,
};
use pallet_issue::Issue;

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
	end: Moment,
	aye: Balance,
	nay: Balance,
}

#[derive(Default, PartialEq, Eq, Clone, Encode, Decode, RuntimeDebug)]
pub struct GovVote<Balance> {
	pub aye: bool,
	pub balance: Balance
}

pub type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as frame_system::Trait>::AccountId>>::Balance;
pub type MomentOf<T> = <T as pallet_timestamp::Trait>::Moment;
pub type ProposalT<T> = Proposal<MomentOf<T>, BalanceOf<T>>;

decl_storage! {
	trait Store for Module<T: Trait> as Gov {
		Alive get(fn alive): bool = true;
		CurrentStep get(fn current_step): u32 = 0;
		OpenedProposal get(fn opened_proposal):  Option<ProposalT<T>>;
		HistoryProposals get(fn history_proposals): Vec<ProposalT<T>>;
		Votes get(fn votes): double_map hasher(twox_64_concat) u32, hasher(twox_64_concat) T::AccountId => GovVote<BalanceOf<T>>;
		VotingOf get(fn voting_of): map hasher(twox_64_concat) u32 => Vec<T::AccountId>;
	}
}

decl_event!(
	pub enum Event<T> where
	AccountId = <T as frame_system::Trait>::AccountId,
	Balance = BalanceOf<T> {
		OpenProposal(u32), // step
		CloseProposal(u32, bool), // step, state
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
	}
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		// Errors must be initialized if they are used by the pallet.
		type Error = Error<T>;

		// Events must be initialized if they are used by the pallet.
		fn deposit_event() = default;

		#[weight = 0]
		pub fn open_next(origin) -> DispatchResult {
			ensure!(Alive::get(), Error::<T>::BrokenConsensus);
			ensure!(<OpenedProposal<T>>::get().is_none(), Error::<T>::UncloseProposal);

			let next_step = CurrentStep::get() + 1;
			let end_time = <pallet_timestamp::Module<T>>::get() + T::VoteBlockTime::get();
			let proposal = Proposal {
				step: next_step,
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
			ensure!(vote.balance <= T::Currency::free_balance(&who), Error::<T>::InsufficientFunds);

			let mut prop = <OpenedProposal<T>>::get().ok_or(Error::<T>::UnExistedProposal)?;
			ensure!(!Self::is_finish(prop.end), Error::<T>::OutdatedProposal);
			ensure!(!<Votes<T>>::contains_key(prop.step, &who), Error::<T>::AlreadyVetoed);

			if vote.aye {
				prop.aye += vote.balance;
			} else {
				prop.nay += vote.balance;
			}

			T::Currency::set_lock(
				GOV_ID,
				&who,
				vote.balance,
				WithdrawReasons::all()
			);

			// change storage
			<Votes<T>>::insert(prop.step, &who, &vote);
			<OpenedProposal<T>>::put(&prop);
			<VotingOf<T>>::mutate(prop.step, |person| person.push(who.clone()));

			Self::deposit_event(RawEvent::Voted(prop.step, who, vote.aye, vote.balance));
			Ok(())
		}

		#[weight = 0]
		pub fn over(origin) -> DispatchResult {
			let _sender = ensure_signed(origin)?;
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

	// TODO more complex rule
	fn judge(props: &ProposalT<T>) -> bool {
		props.aye > props.nay
	}
}
