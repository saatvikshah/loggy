# loggy

Merge and manipulate log files like theres no tomorrow

## MVP Features

- Merge timestamped log files - [sample use case](https://stackoverflow.com/questions/15866772/merging-multiple-log-files-by-date-including-multilines)
- Apply time range queries - [sample use case](https://stackoverflow.com/q/7575267/3656081)

## Build

```
cabal build
cabal run loggy -- -i file1.txt -i file2.txt@"%H:%M:%S" -i file3.txt --format "%H-%M-%S"
```

## Tool Description

```
Usage: loggy --format DATE_FORMAT (-i FILE(with optional @DATE_FORMAT))
  Merge and manipulate log files.

Available options:
  -h,--help                Show this help text
  --format DATE_FORMAT     common date format
  -i FILE(with optional @DATE_FORMAT)
                           Name of the file and its optional date format
                           separated by @. Repeatable arg.
```

## Rough Checkpoints

- [ ] Checkpoint 2: Improvements and Verification
  - [x] optparse improvements (figure out good command line syntax)
  - [x] Merging multiple log files
    - [x] With same format
    - [x] With different formats
  - [x] Add unit tests(HUnit)
  - [x] Remove "error" on parse issues, replace with "Maybe"
  - [x] Adding quickcheck testing(how to model strftime/log file inputs)
  - [x] Add -Werror flag
  - [ ] `fileembed` with actual data + setup of regression tests
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
  - [ ] Group lines without timestamps
  - [ ] Reading log files in parallel
  - [ ] Look into doctest to clean up unit tests
