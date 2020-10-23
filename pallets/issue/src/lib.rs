#![cfg_attr(not(feature = "std"), no_std)]

use sp_std::prelude::*;
use frame_support::{decl_module, ensure, decl_storage, decl_event, decl_error, dispatch,
					traits::{Currency}};
use frame_system::{ensure_root};
use dispatch::DispatchResult;
use codec::{Decode, Encode};
use sp_runtime::{
	RuntimeDebug,
	traits::{Zero},
};

pub(crate) const LOG_TARGET: &'static str = "issue";

macro_rules! log {
	($level:tt, $patter:expr $(, $values:expr)* $(,)?) => {
		frame_support::debug::$level!(
			target: crate::LOG_TARGET,
			$patter $(, $values)*
		)
	};
}

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

pub trait Trait: frame_system::Trait {
	type Event: From<Event<Self>> + Into<<Self as frame_system::Trait>::Event>;
	type Currency: Currency<Self::AccountId>;
}

pub trait Issue<Balance> {
	fn remain() -> Balance;
	fn consume() -> bool;
	fn cut(amount: Balance) -> Balance;
}

// Issue information
#[derive(Default, PartialEq, Eq, Clone, Encode, Decode, RuntimeDebug)]
pub struct IssueData<Balance> {
	total: Balance,
	unreleased: Balance,
}

// Stage information
#[derive(Default, PartialEq, Eq, Clone, Encode, Decode, RuntimeDebug)]
pub struct IssueStage<Balance> {
	name: Vec<u8>,
	release: Balance,
	closed: bool,
}

pub type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as frame_system::Trait>::AccountId>>::Balance;
type IssueDataT<T> = IssueData<BalanceOf<T>>;
type IssueStageT<T> = IssueStage<BalanceOf<T>>;

decl_storage! {
	trait Store for Module<T: Trait> as Issue {
		Data get(fn data) : IssueDataT<T>;
		Stages get(fn stages) : Vec<IssueStageT<T>>;
		Remain get(fn remain) : BalanceOf<T>;
	}
	add_extra_genesis {
        config(stgs):  Vec<(Vec<u8>, BalanceOf<T>)>;
        build(|config: &GenesisConfig<T>|  {
        	let mut issue_stages = vec![];
        	let mut total:BalanceOf<T> = Zero::zero();
			for stg in config.stgs.iter() {
				assert!( stg.1 > Zero::zero(), "Stage balance not zero");
				issue_stages.push(IssueStage {
					name: stg.0.clone(),
					release: stg.1.clone(),
					closed: false,
				});
				total += stg.1;
			}
			<Data<T>>::set(IssueData {
				total: total,
				unreleased: total,
			});
			<Stages<T>>::set(issue_stages);
			<Module<T>>::_consume();
        });
    }
}

decl_event!(
	pub enum Event<T> where Balance = BalanceOf<T> {
		AddStage(Vec<u8>, Balance),
	}
);

decl_error! {
	pub enum Error for Module<T: Trait>  {
		ZeroRelease,
	}
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		// Errors must be initialized if they are used by the pallet.
		type Error = Error<T>;

		// Events must be initialized if they are used by the pallet.
		fn deposit_event() = default;

		#[weight = 0]
		pub fn add_stage(origin, name: Vec<u8>, amount: BalanceOf<T>) -> DispatchResult {
			let _root = ensure_root(origin)?;
			ensure!(amount > Zero::zero(), Error::<T>::ZeroRelease);
			Self::_add_stage(name, amount);
			Ok(())
		}
	}
}

impl<T: Trait> Module<T> {
	pub fn _add_stage(name: Vec<u8>, amount: BalanceOf<T>) {
		let stage = IssueStage {
			name: name.clone(),
			release: amount,
			closed: false,
		};
		<Stages<T>>::mutate(|stgs| stgs.push(stage));
		<Data<T>>::mutate(|data| {
			data.total += amount;
			data.unreleased += amount;
		});
		Self::deposit_event(RawEvent::AddStage(name, amount));
	}

	pub fn _consume() -> bool {
		// find next stage
		let mut stages = <Stages<T>>::get();
		let mut release = Zero::zero();
		for s in &mut stages {
			if !s.closed {
				release = s.release;
				s.closed = true;
				break;
			}
		}

		if release == Zero::zero() {
			return false;
		}

		<Data<T>>::mutate(|data| { data.unreleased -= release; });
		<Remain<T>>::mutate(|d| { *d += release; });
		<Stages<T>>::put(stages);
		true
	}

	pub fn _cut(balance: BalanceOf<T>) -> BalanceOf<T> {
		log!(info, "cut balance: {:?}", balance);

		let mut amount = balance;
		<Remain<T>>::mutate(|d| {
			if *d > amount {
				*d = *d - amount;
			} else {
				amount = *d;
				*d = Zero::zero();
			}
		});
		amount
	}
}

impl<T: Trait> Issue<BalanceOf<T>> for Module<T> {
	fn remain() -> BalanceOf<T> {
		<Remain<T>>::get()
	}
	fn consume() -> bool {
		Self::_consume()
	}
	fn cut(balance: BalanceOf<T>) -> BalanceOf<T> {
		Self::_cut(balance)
	}
}