use std::fmt;
use std::marker::PhantomData;

pub struct Arena<T>(Vec<T>);

pub struct Id<T> {
	idx: u32,
	phantom: PhantomData<fn() -> T>,
}

pub struct ArenaMap<K, V>(Vec<Option<V>>, PhantomData<Id<K>>);

impl<T> Arena<T> {
	pub fn new() -> Arena<T> {
		Arena(Vec::new())
	}

	pub fn alloc(&mut self, val: T) -> Id<T> {
		let idx = self.0.len() as u32;
		self.0.push(val);
		Id { idx, phantom: PhantomData }
	}

	pub fn get(&self, id: Id<T>) -> &T {
		&self.0[id.idx as usize]
	}

	pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
		&mut self.0[id.idx as usize]
	}
}

impl<T> Default for Arena<T> {
	fn default() -> Arena<T> {
		Arena::new()
	}
}

impl<T> Id<T> {
	pub fn to_raw(self) -> u32 {
		self.idx
	}
}

impl<K, V> ArenaMap<K, V> {
	pub fn new() -> ArenaMap<K, V> {
		ArenaMap(Vec::new(), PhantomData)
	}

	pub fn insert(&mut self, key: Id<K>, value: V) {
		let idx = key.idx as usize;
		self.0.resize_with(idx + 1, || None);
		match &mut self.0[idx] {
			v @ None => *v = Some(value),
			Some(_) => panic!("key Id({idx}) already present in ArenaMap"),
		}
	}

	#[track_caller]
	pub fn get(&self, key: Id<K>) -> Option<&V> {
		self.0.get(key.idx as usize)?.as_ref()
	}
}

impl<K, V> Default for ArenaMap<K, V> {
	fn default() -> ArenaMap<K, V> {
		ArenaMap::new()
	}
}

impl<K, V> fmt::Debug for ArenaMap<K, V>
where
	V: fmt::Debug,
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug_map = f.debug_map();

		for (idx, slot) in self.0.iter().enumerate() {
			if let Some(v) = slot {
				let id: Id<K> = Id { idx: idx as u32, phantom: PhantomData };
				debug_map.entry(&id as &dyn fmt::Debug, v);
			}
		}

		debug_map.finish()
	}
}

impl<T> Clone for Id<T> {
	fn clone(&self) -> Id<T> {
		Self { idx: self.idx, phantom: PhantomData }
	}
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
	fn eq(&self, other: &Self) -> bool {
		self.idx == other.idx
	}
}

impl<T> fmt::Debug for Id<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Id::<{}>({})", std::any::type_name::<T>(), self.idx)
	}
}
