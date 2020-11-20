#![cfg_attr(not(feature = "std"), no_std)]

use core::convert::{From, Into, TryInto};

use codec::{Decode, Encode};
use dispatch::DispatchResult;
use frame_support::{
	debug, decl_error, decl_event, decl_module, decl_storage, dispatch, ensure, traits::Currency,
};
use frame_system::{ensure_signed};
use sp_runtime::{traits::Zero, RuntimeDebug};
use sp_std::prelude::*;

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
	fn get_operate_account() -> Vec<u8>;
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

// OperateIssue information
#[derive(Default, PartialEq, Eq, Clone, Encode, Decode, RuntimeDebug)]
pub struct OperateAccountT {
	address: Vec<u8>,
}

pub type BalanceOf<T> =
	<<T as Trait>::Currency as Currency<<T as frame_system::Trait>::AccountId>>::Balance;
type IssueDataT<T> = IssueData<BalanceOf<T>>;
type IssueStageT<T> = IssueStage<BalanceOf<T>>;

decl_storage! {
	trait Store for Module<T: Trait> as Issue {
		Data get(fn data) : IssueDataT<T>;
		Stages get(fn stages) : Vec<IssueStageT<T>>;
		Remain get(fn remain) : BalanceOf<T>;
		OperateAccount get(fn operate_account) : OperateAccountT;
		OperateAdmin get(fn operate_admin) : OperateAccountT;
	}
	add_extra_genesis {
		config(stgs):  Vec<(Vec<u8>, BalanceOf<T>)>;
		config(operate_account): Vec<u8>;
		config(operate_admin): Vec<u8>;
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

			//get operate account from chain_spec
			let init_acc = OperateAccountT{
				address:config.operate_account.clone()
			};

			//init set operate account
			<OperateAccount>::set(init_acc);

			//get operate admin from chain_spec
			let init_op_admin = OperateAccountT{
				address:config.operate_admin.clone()
			};

			//init set operate account
			<OperateAdmin>::set(init_op_admin);

			//first round issue
			<Module<T>>::_consume();
		});
	}
}

decl_event!(
	pub enum Event<T>
	where
		Balance = BalanceOf<T>,
	{
		AddStage(Vec<u8>, Balance),
		ChangeOperate(Vec<u8>, Vec<u8>),
	}
);

decl_error! {
	pub enum Error for Module<T: Trait>  {
		ZeroRelease,
		NotOperateAdmin,
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
			let _sender = ensure_signed(origin)?;
			ensure!(Self::_verify_operate_admin(_sender), Error::<T>::NotOperateAdmin);
			ensure!(amount > Zero::zero(), Error::<T>::ZeroRelease);
			Self::_add_stage(name, amount);
			Ok(())
		}

		//change operate account, root
		#[weight = 0]
		pub fn change_operate(origin, account: Vec<u8>) -> DispatchResult {
			let _sender = ensure_signed(origin)?;
			ensure!(Self::_verify_operate_admin(_sender), Error::<T>::NotOperateAdmin);
			Self::_change_operate(account);
			Ok(())
		}
	}

}

impl<T: Trait> Module<T> {
	fn get_account_data(receiver: &[u8]) -> [u8; 32] {
		let decoded_ss58 = bs58::decode(receiver).into_vec().unwrap();

		// todo, decoded_ss58.first() == Some(&42) || Some(&6) || ...
		if decoded_ss58.len() == 35 {
			let mut data = [0u8; 32];
			data.copy_from_slice(&decoded_ss58[1..33]);
			data
		} else {
			[0; 32]
		}
	}

	fn into_account(data: [u8; 32]) -> T::AccountId {
		T::AccountId::decode(&mut &data[..]).unwrap()
	}

	fn _verify_operate_admin(sender: T::AccountId) -> bool {
		let account_u8 = <OperateAdmin>::get().address;
		let account = account_u8.as_slice();

		let foo = Self::get_account_data(account);
		let foo = Self::into_account(foo);
		if foo == sender {
			true
		} else {
			false
		}
	}

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

	fn _change_operate(new_account: Vec<u8>) {
		let old_operate = <OperateAccount>::get();

		//save 0-9 a-z A-Z
		let mut right_account = Vec::new();
		for u in new_account.clone() {
			if (u >= 48 && u <= 57) || (u >= 65 && u <= 90) || (u >= 97 && u <= 122) {
				right_account.push(u);
			}
		}

		let change_acc = OperateAccountT {
			address: right_account.clone(),
		};
		<OperateAccount>::set(change_acc.clone());
		Self::deposit_event(RawEvent::ChangeOperate(old_operate.address, change_acc.address))
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

		//get operate account
		let account = <OperateAccount>::get().address;
		let account = account.as_slice();

		let foo = Self::get_account_data(account);
		let foo = Self::into_account(foo);
		debug::error!(target: "pallet-issue", "====####==== {:?}", foo);

		//issue coin to operate account
		T::Currency::deposit_creating(&foo, release);
		// T::Currency::deposit_into_existing(&foo, release);

		<Data<T>>::mutate(|data| {
			data.unreleased -= release;
		});

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

	fn get_operate_account() -> Vec<u8> {
		<OperateAccount>::get().address
	}
}
