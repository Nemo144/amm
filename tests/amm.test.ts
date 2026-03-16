import { Cl } from "@stacks/transactions";
import { describe, expect, it, beforeEach } from "vitest";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const alice = accounts.get("wallet_1")!;
const bob = accounts.get("wallet_2")!;
const charlie = accounts.get("wallet_3")!;

//assigning the two mock token addresses to variables for easier use
const mockTokenOne = Cl.contractPrincipal(deployer, "mock-token");
const mockTokenTwo = Cl.contractPrincipal(deployer, "mock-token-2");

//setting up few helpers to make it easier to call the four public functions and the helper function `get-pool-id`

//create pool helper function
function createPool() {
  return simnet.callPublicFn(
    "amm",
    "create-pool",
    [mockTokenOne, mockTokenTwo, Cl.uint(500)],
    alice,
  );
}

function addLiquidity(account: string, amount0: number, amount1: number) {
  return simnet.callPublicFn(
    "amm",
    "add-liquidity",
    [
      mockTokenOne,
      mockTokenTwo,
      Cl.uint(500),
      Cl.uint(amount0),
      Cl.uint(amount1),
      Cl.uint(0),
      Cl.uint(0),
    ],
    account,
  );
}

function removeLiquidity(account: string, liquidity: number) {
  return simnet.callPublicFn(
    "amm",
    "remove-liquidity",
    [mockTokenOne, mockTokenTwo, Cl.uint(500), Cl.uint(liquidity)],
    account,
  );
}

function swap(account: string, inputAmount: number, zeroForOne: boolean) {
  return simnet.callPublicFn(
    "amm",
    "swap",
    [
      mockTokenOne,
      mockTokenTwo,
      Cl.uint(500),
      Cl.uint(inputAmount),
      Cl.bool(zeroForOne),
    ],
    account,
  );
}

function getPoolId() {
  return simnet.callReadOnlyFn(
    "amm",
    "get-pool-id",
    [
      Cl.tuple({
        "token-0": mockTokenOne,
        "token-1": mockTokenTwo,
        fee: Cl.uint(500),
      }),
    ],
    alice,
  );
}
