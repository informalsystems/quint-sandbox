module evm {
    // An address is simply is string. The particular format is not essential.
    type Address = str
    // the special zero address, which corresponds to the address 0 in EVM
    pure val ZERO_ADDRESS = "0"

    // An EVM integer is 256 bits.
    // We are using big integers and check for overflows manually.
    type Uint = int
    pure val MAX_UINT = 2^256 - 1
    pure def isUint(i: int): bool = 0 <= i and i <= MAX_UINT
    pure def toUint(i: int): Uint = i % (2^256)

    // An auxilliary definition similar to Solidity's require
    pure def require(cond: bool, msg: str): str = {
        if (cond) "" else msg
    }

    // an easy way to chain require calls
    pure def andRequire(prevErr: str, cond: bool, msg: str): str = {
        if (prevErr != "") prevErr else require(cond, msg)
    }
}