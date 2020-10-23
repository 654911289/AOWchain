use crate::{Module, Trait};
use sp_core::H256;
use frame_support::{impl_outer_origin, parameter_types, weights::Weight};
use sp_runtime::{
	traits::{BlakeTwo256, IdentityLookup}, testing::Header, Perbill,
};
use frame_system as system;

impl_outer_origin! {
	pub enum Origin for Test {}
}

// Configure a mock runtime to test the pallet.

#[derive(Clone, Eq, PartialEq)]
pub struct Test;
parameter_types! {
	pub const BlockHashCount: u64 = 250;
	pub const MaximumBlockWeight: Weight = 1024;
	pub const MaximumBlockLength: u32 = 2 * 1024;
	pub const AvailableBlockRatio: Perbill = Perbill::from_percent(75);
}

impl system::Trait for Test {
	type BaseCallFilter = ();
	type Origin = Origin;
	type Call = ();
	type Index = u64;
	type BlockNumber = u64;
	type Hash = H256;
	type Hashing = BlakeTwo256;
	type AccountId = u64;
	type Lookup = IdentityLookup<Self::AccountId>;
	type Header = Header;
	type Event = ();
	type BlockHashCount = BlockHashCount;
	type MaximumBlockWeight = MaximumBlockWeight;
	type DbWeight = ();
	type BlockExecutionWeight = ();
	type ExtrinsicBaseWeight = ();
	type MaximumExtrinsicWeight = MaximumBlockWeight;
	type MaximumBlockLength = MaximumBlockLength;
	type AvailableBlockRatio = AvailableBlockRatio;
	type Version = ();
	type ModuleToIndex = ();
	type AccountData = pallet_balances::AccountData<u64>;
	type OnNewAccount = ();
	type OnKilledAccount = ();
	type SystemWeightInfo = ();
}

parameter_types! {
	pub const MinimumPeriod: u64 = 5;
	pub const VoteBlockTime: u64 = 2;
	pub const ExistentialDeposit: u64 = 1;
}
impl pallet_timestamp::Trait for Test {
	type Moment = u64;
	type OnTimestampSet = ();
	type MinimumPeriod = MinimumPeriod;
	type WeightInfo = ();
}

impl pallet_balances::Trait for Test {
	type Balance = u64;
	type DustRemoval = ();
	type Event = ();
	type ExistentialDeposit = ExistentialDeposit;
	type AccountStore = frame_system::Module<Test>;
	type WeightInfo = ();
}

impl pallet_issue::Trait for Test {
	type Event = ();
	type Currency = Balances;
}

impl Trait for Test {
	type Event = ();
	type Currency = Balances;
	type Issue = Issue;
	type VoteBlockTime = VoteBlockTime;
}

pub type System = frame_system::Module<Test>;
pub type Balances = pallet_balances::Module<Test>;
pub type Timestamp = pallet_timestamp::Module<Test>;
pub type Issue = pallet_issue::Module<Test>;
pub type GovModule = Module<Test>;

// Build genesis storage according to the mock runtime.
pub fn new_test_ext() -> sp_io::TestExternalities {
	let mut t = system::GenesisConfig::default().build_storage::<Test>().unwrap();
	pallet_balances::GenesisConfig::<Test> {
		balances: vec![
				(10, 50),
				(20, 50),
				(30, 50)],
	}.assimilate_storage(&mut t).unwrap();
	pallet_issue::GenesisConfig::<Test> {
		stgs: vec![
			(vec![0u8], 50),
			(vec![1u8], 50)],
	}.assimilate_storage(&mut t).unwrap();
	t.into()
}
