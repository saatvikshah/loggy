# loggy

Merge and manipulate log files like theres no tomorrow

## MVP Features

- Merge timestamped log files - [sample use case](https://stackoverflow.com/questions/15866772/merging-multiple-log-files-by-date-including-multilines)
- Apply time range queries - [sample use case](https://stackoverflow.com/q/7575267/3656081)

## Build

```
cabal build
cabal run loggy -- --text1 file1.txt --text2 file2.txt --format "%H:%M:%S"
```

## Rough Checkpoints

- [ ] Checkpoint 2: Minor improvements and Verification
  - [ ] optparse improvements (figure out good command line syntax)
  - [ ] Merging multiple log files
    - [ ] With same format
    - [ ] With different formats
  - [x] Add unit tests(HUnit)
  - [ ] Adding quickcheck testing (minor + time consuming)
  - [ ] Add Github CI (optional)
- [ ] Checkpoint 3: Performance
  - [ ] Use Vector/Text
  - [ ] Use heap for low memory consumption/Use streaming library
  - [ ] Benchmarks(optional)
- [ ] Checkpoint 4: Stretch goals
  - [ ] Pull in remote files
  - [ ] Terminal coloring
    - [ ] Filename prefixing in output
    - [ ] Text marked as a specific color (eg. log warn levels)
  - [ ] Print output in a range of timestamps
