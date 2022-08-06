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
}

impl<T> Default for Arena<T> {
	fn default() -> Arena<T> {
		Arena::new()
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

	pub fn get(&self, key: Id<K>) -> &V {
		self.0[key.idx as usize].as_ref().unwrap()
	}
}

impl<K, V> Default for ArenaMap<K, V> {
	fn default() -> ArenaMap<K, V> {
		ArenaMap::new()
	}
}

impl<T> Clone for Id<T> {
	fn clone(&self) -> Id<T> {
		Self { idx: self.idx, phantom: PhantomData }
	}
}

impl<T> Copy for Id<T> {}
