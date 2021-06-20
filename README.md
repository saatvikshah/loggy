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

- [ ] Checkpoint 2: Improvements and Verification
  - [ ] optparse improvements (figure out good command line syntax)
  - [x] Merging multiple log files
    - [x] With same format
    - [x] With different formats
  - [x] Add unit tests(HUnit)
  - [ ] Remove "error" on parse issues, replace with "Maybe"
  - [ ] Look into doctest to clean up unit tests
  - [ ] Add -Weverything flag
  - [ ] Add Github CI (optional)
- [ ] Checkpoint 3: Performance
  - [ ] Use Vector/Text
  - [ ] Use heap for low memory consumption/Use streaming library
  - [ ] Benchmarks(optional)
- [ ] Checkpoint 4: Stretch goals
  - [ ] `fileembed` with actual data + setup of regression tests
  - [ ] Adding quickcheck testing(how to model strftime/log file inputs)
  - [ ] Pull in remote files
  - [ ] Terminal coloring
    - [ ] Filename prefixing in output
    - [ ] Text marked as a specific color (eg. log warn levels)
  - [ ] Print output in a range of timestamps
  - [ ] Group lines without timestamps
  - [ ] Reading log files in parallel
