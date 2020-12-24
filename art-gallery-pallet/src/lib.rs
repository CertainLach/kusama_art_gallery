//! # Art Gallery
//! The module provides implementations for art gallery with
//! non-fungible-tokens.
//!
//! - [`Config`](./trait.Config.html)
//! - [`Call`](./enum.Call.html)
//! - [`Module`](./struct.Module.html)
//!
//! ## Overview
//!
//! This module tightly coupled with NFT module provides basic functions to
//! manage Art Gallery with tree hierarchy.
//!
//! ### Module Functions
//!
//! - `mint` - Mint NFT(non fungible token)
//! - `burn` - Burn NFT(non fungible token)
//! - `transfer` - Change owner for NFT(non fungible token) with tree hierarchy
//! limitation
//! - `assign` - Add NFT(non fungible token) to gallery hierarchy
//! - `unassign` - Remove NFT(non fungible token) from gallery hierarchy
//! - `mint_and_assign` - Mint NFT(non fungible token) and add to gallery
//! hierarchy

use codec::{Decode, Encode};
use frame_support::{decl_error, decl_module, decl_storage, ensure, fail};
use frame_system::ensure_signed;
use orml_nft::{self as nft};
use sp_runtime::DispatchResult;
use sp_std::vec::Vec;

pub mod relations;

#[derive(Encode, Decode, Eq, Debug, Clone, PartialEq)]
pub enum TokenType {
	Art,
	SubCollection,
}

impl Default for TokenType {
	fn default() -> Self {
		Self::Art
	}
}

pub trait Trait: frame_system::Config + nft::Config {}

decl_error! {
	/// Error for art gallery module.
	pub enum Error for Module<T: Trait> {
		/// Sub-collection must be empty before burn it
		SubCollectionNotEmtpy,
		/// Sub-collection and all childs must be owned
		SubCollectionHasAnotherOwner,
		/// Token(ClassId, TokenId) not found
		TokenNotFound,
		/// Tokens must be in one collection
		TokensInDifferentClasses,
		/// Only unassigned tokens can be assigned
		CannotAssignTokenInHierarchy,
		/// Only sub-collection can be parent
		AssignToNotSubCollection,
	}
}

decl_storage! {
	trait Store for Module<T: Trait> as ArtGallery {
		/// Tokens relations storage
		pub Relations get(fn relations): map hasher(twox_64_concat) T::ClassId => Vec<(T::TokenId, T::TokenId)>;
		/// Extended token info.
		///
		/// Returns `None` if class info not set or removed.
		pub TokenTypes get(fn token_types): double_map hasher(twox_64_concat) T::ClassId, hasher(twox_64_concat) T::TokenId => Option<TokenType>;
	}
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {

		type Error = Error<T>;

		#[weight = 0]
		pub fn mint(origin,
				owner: T::AccountId,
				class_id: T::ClassId,
				metadata: Vec<u8>,
				data: T::TokenData,
				token_type: TokenType) -> DispatchResult {
			let _who = ensure_signed(origin)?;
			let token_id = nft::Module::<T>::mint(&owner, class_id, metadata, data).unwrap();
			<TokenTypes<T>>::insert(class_id, token_id, token_type);
			Ok(())
		}

		#[weight = 0]
		pub fn burn(origin, owner: T::AccountId, token: (T::ClassId, T::TokenId)) -> DispatchResult {
			let _who = ensure_signed(origin)?;
			let token_type = match <TokenTypes<T>>::get(token.0, token.1) {
				Some(t) => t,
				None => fail!(Error::<T>::TokenNotFound),
			};
			if token_type == TokenType::SubCollection {

				// ensure sub-collection is empty
				let mut rels = <Relations<T>>::get(token.0);
				ensure!(relations::get_flatten_childs_subtree(&mut rels, token.1).is_empty(), Error::<T>::SubCollectionNotEmtpy);

				// update relations
				relations::remove(&mut rels, token.1);
				<Relations<T>>::insert(token.0, rels);
			}

			nft::Module::<T>::burn(&owner, token)?;
			Ok(())
		}

		#[weight = 0]
		pub fn transfer(origin,
				from: T::AccountId,
				to: T::AccountId,
				token: (T::ClassId, T::TokenId)) -> DispatchResult {
			let _who = ensure_signed(origin)?;

			let mut rels = <Relations<T>>::get(token.0);
			let childs = relations::get_flatten_childs_subtree(&mut rels, token.1);
			for tk in childs {
				ensure!(nft::Module::<T>::is_owner(&from, (token.0, tk)), Error::<T>::SubCollectionHasAnotherOwner);
			}

			nft::Module::<T>::transfer(&from, &to, token)?;
			Ok(())
		}

		#[weight = 0]
		pub fn assign(origin,
				parent: (T::ClassId, T::TokenId),
				child: (T::ClassId, T::TokenId)) -> DispatchResult {
			let _who = ensure_signed(origin)?;
			ensure!(parent.0 == child.0, Error::<T>::TokensInDifferentClasses);

			// check type
			let token_type = match <TokenTypes<T>>::get(parent.0, parent.1) {
				Some(t) => t,
				None => fail!(Error::<T>::TokenNotFound),
			};
			ensure!(token_type == TokenType::SubCollection, Error::<T>::AssignToNotSubCollection);

			let mut rels = <Relations<T>>::get(parent.0);
			ensure!(relations::assign(&mut rels, parent.1, child.1), Error::<T>::CannotAssignTokenInHierarchy);
			<Relations<T>>::insert(parent.0, rels);

			Ok(())
		}

		#[weight = 0]
		pub fn unassign(origin,
				token: (T::ClassId, T::TokenId)) -> DispatchResult {
			let _who = ensure_signed(origin)?;

			let mut rels = <Relations<T>>::get(token.0);
			ensure!(relations::unassign(&mut rels, token.1), Error::<T>::TokenNotFound);
			<Relations<T>>::insert(token.0, rels);

			Ok(())
		}

		#[weight = 0]
		pub fn assign_and_mint(origin,
				owner: T::AccountId,
				class_id: T::ClassId,
				metadata: Vec<u8>,
				data: T::TokenData,
				token_type: TokenType,
				parent: (T::ClassId, T::TokenId)) -> DispatchResult {
			let _who = ensure_signed(origin)?;

			let token_id = nft::Module::<T>::mint(&owner, class_id, metadata, data).unwrap();
			<TokenTypes<T>>::insert(class_id, token_id, token_type);

			let mut rels = <Relations<T>>::get(parent.0);
			ensure!(relations::assign(&mut rels, parent.1, token_id), Error::<T>::CannotAssignTokenInHierarchy);
			<Relations<T>>::insert(parent.0, rels);

			Ok(())
		}
	}
}
