# Simple blockchain in Ada

## Building

``` bash
$ make build
```

## Running

``` bash
$ make run
```

## Building and running

``` bash
$ make
```

## Example output

```
Simple blockchain demo

Mining first block...
Block mined.

Mining second block...
Block mined.

Mining third block...
Block mined.

Is blockchain valid? TRUE

Printing blockchain...
Blockchain - difficulty:  6, blocks:  3
Hash: 000000f78298d2028737f802a82edb955e0e4cfdc06b514325da2a037f41b754, Previous hash: 0000000000000000000000000000000000000000000000000000000000000000, Timestamp: 2018-06-14 13:26:11, Nonce:  10942584, Data: First block
Hash: 000000ddfddf305f15fefdada0939139ea33dc94bca1e57eda5f16d902fec55f, Previous hash: 000000f78298d2028737f802a82edb955e0e4cfdc06b514325da2a037f41b754, Timestamp: 2018-06-14 13:26:17, Nonce:  6008350, Data: Second block
Hash: 000000e4530387b122e91de4fedc84b77406a819713c9601b09b29695cb6e336, Previous hash: 000000ddfddf305f15fefdada0939139ea33dc94bca1e57eda5f16d902fec55f, Timestamp: 2018-06-14 13:26:32, Nonce:  17905780, Data: Third block
```
