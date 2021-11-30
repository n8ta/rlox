use crate::Value;

const MIN_SIZE: usize = 1000;

struct BumpAllocator {
    envs: Vec<Vec<Value>>
}

// impl BumpAllocator {
//     fn alloc(&mut self, literals: usize) {
//
//     }
//     fn free(&mut self, )
// }