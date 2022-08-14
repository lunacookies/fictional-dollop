# fictional-dollop

Another one of those ‘modern C’ languages like Zig and Odin,
but this time with a focus on high quality, performant tooling.
Knowing how these things usually pan out I doubt it’ll get anywhere,
but it’s a fun project regardless.

Please please if you’re interested in this project
email me (address in profile)
or make some pull requests,
I’d love to get some help and second opinions
on such a large undertaking.

## Architecture

1. Parse all files
2. Create a `raw_index::Stub` for each file,
   gathering them all into a `raw_index::Index`
3. Resolve every stub (producing a `resolved_index::Stub`),
   gathering them all into a `resolved_index::Index`
4. For every file, resolve and typecheck the bodies of all items
   using that file’s parse tree and resolved index,
   storing a map from definitions to bodies
5. Generate code for all functions

Each step can be parallelized across files without locking.
By tracking which files every file depends on during steps 3 and 4,
incremental compilation and IDE support can be implemented.

When an edit to a file arrives,
we remove the corresponding raw stub from the raw index
and resolved stub from the resolved index.
Next, we generate a new raw stub for the file and insert it into the index.
We then rerun steps 2, 3 and 4 for the file and the files that reference it.
