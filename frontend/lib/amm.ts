import { STACKS_TESTNET } from "@stacks/network";
import {
  boolCV,
  bufferCV,
  Cl,
  cvToHex,
  fetchCallReadOnlyFunction,
  hexToCV,
  principalCV,
  PrincipalCV,
  uintCV,
  UIntCV,
} from "@stacks/transactions";
import { uint } from "@stacks/transactions/dist/cl";
import { buffer } from "stream/consumers";

//define the contract address, name and principal
const AMM_CONTRACT_ADDRESS = "ST3PEN3P9CDM5HNDW4VKRWY5T456AK990AZRKA5E";
const AMM_CONTRACT_NAME = "amm";
const AMM_CONTRACT_PRINCIPAL = `$(AMM_CONTRACT_ADDRESS).$(AMM_CONTRACT_NAME)`;

//define the contract events type
type ContractEvent = {
  event_index: number;
  event_type: string;
  tx_id: string;
  contract_log: {
    contract_id: string;
    topic: string;
    value: {
      hex: string;
      repr: string;
    };
  };
};

//PoolCV is a struct type, represents the clarity value version of the tuple gotten from the `pools` map in the clarity smart contract
type PoolCV = {
  "token-0": PrincipalCV;
  "token-1": PrincipalCV;
  fee: UIntCV;
  liquidity: UIntCV;
  "balance-0": UIntCV;
  "balance-1": UIntCV;
};

//standard typescript type for use across the frontend
export type Pool = {
  id: string;
  "token-0": string;
  "token-1": string;
  fee: number;
  liquidity: number;
  "balance-0": number;
  "balance-1": number;
};

//define the getAllPools function that fetches all pools that exist on the contract with emphasis on the "create-pool" print event!
export async function getAllPools() {
  //initial value of the offset will be zero
  let offset = 0;

  //the "done" variable evaluates to false
  let done = false;

  //define the pools array
  const pools: Pool[] = [];

  //50 events can be fetched at a time, so we run a loop until we have fetched all events
  while (!done) {
    //define the url
    const url = `http://api.testnet.hiro.so/extended/v1/contract/${AMM_CONTRACT_PRINCIPAL}/events?limit=50&offset=${offset}`;

    //define the response(events) from the url
    const events = (await fetch(url).then((res) => res.json()))
      .results as ContractEvent[];

    //if at any point less than 50 events is being returned then that is the last iteration
    if (events.length < 50) {
      done = true;
    }

    //filter events from the smart contract to only those which are smart_contract_log
    const filteredEvents = events.filter((event: ContractEvent) => {
      return event.event_type === "smart_contract_log";
    });

    for (const event of filteredEvents) {
      const contractLog = event.contract_log;
      if (contractLog.contract_id !== AMM_CONTRACT_PRINCIPAL) continue;
      if (contractLog.topic !== "print") continue;

      //for each event, only the ones with the action = "create-pool" is the concern
      const data = hexToCV(contractLog.value.hex);
      if (data.type !== "tuple") continue;
      if (data.value["action"] === undefined) continue;
      if (data.value["action"].type !== "ascii") continue;
      if (data.value["action"]["value"] !== "create-pool") continue;
      if (data.value["data"].type !== "tuple") continue;

      //define the poolInitialdata
      const poolInitialData = data.value["data"].value as PoolCV;

      //get the poolId fromt the poolInitialData
      const poolIdResult = await fetchCallReadOnlyFunction({
        contractAddress: AMM_CONTRACT_ADDRESS,
        contractName: AMM_CONTRACT_NAME,
        functionName: "get-pool-id",
        functionArgs: [
          Cl.tuple({
            "token-0": poolInitialData["token-0"],
            "token-1": poolInitialData["token-1"],
            fee: poolInitialData.fee,
          }),
        ],
        senderAddress: AMM_CONTRACT_ADDRESS,
        network: STACKS_TESTNET,
      });
      if (poolIdResult.type !== "buffer") continue;

      //define the poolId
      const poolId = poolIdResult.value;

      //get the pool data from the pool id
      const poolDataResult = await fetchCallReadOnlyFunction({
        contractAddress: AMM_CONTRACT_ADDRESS,
        contractName: AMM_CONTRACT_NAME,
        functionName: "get-pool-data",
        functionArgs: [poolIdResult],
        senderAddress: AMM_CONTRACT_ADDRESS,
        network: STACKS_TESTNET,
      });

      if (poolDataResult.type !== "ok") continue;
      if (poolDataResult.value.type !== "some") continue;
      if (poolDataResult.value.value.type !== "tuple") continue;

      //define the poolData
      const poolData = poolDataResult.value.value.value as PoolCV;

      //convert the pool data to a pool object
      const pool: Pool = {
        id: poolId,
        "token-0": poolInitialData["token-0"].value,
        "token-1": poolInitialData["token-1"].value,
        fee: parseInt(poolInitialData["fee"].value.toString()),
        liquidity: parseInt(poolInitialData["liquidity"].value.toString()),
        "balance-0": parseInt(poolInitialData["balance-0"].value.toString()),
        "balance-1": parseInt(poolInitialData["balance-1"].value.toString()),
      };

      //define the pools array
      pools.push(pool);

      //update the offset
      offset = event.event_index;
    }
  }
  return pools;
}

//createPool function for the transaction to the contract
export async function createPool(token0: string, token1: string, fee: number) {
  //convert the tokens to hex
  const token0Hex = cvToHex(principalCV(token0));
  const token1Hex = cvToHex(principalCV(token1));

  //sort the order of the tokens
  if (token0Hex > token1Hex) {
    [token0, token1] = [token1, token0];
  }

  //define the txOptions
  const txOptions = {
    contractAddress: AMM_CONTRACT_ADDRESS,
    contractName: AMM_CONTRACT_NAME,
    functionName: "create-pool",
    functionArgs: [principalCV(token0), principalCV(token1), uintCV(fee)],
  };
  return txOptions;
}

//addLiquidity function for the transaction to the contract
export async function addLiquidity(
  pool: Pool,
  amount0: number,
  amount1: number,
) {
  //check for the liquidity of both amounts
  if (amount0 === 0 || amount1 === 1) {
    throw new Error("cannot add liquidity with 0 amount");
  }

  //if it is not initial liquidity, then the amounts are added in ratio of the price
  if (pool.liquidity > 0) {
    //define the poolRatio
    const poolRatio = pool["balance-0"] / pool["balance-1"];

    //define the idealAmount1
    const idealAmount1 = Math.floor(amount0 / poolRatio);

    //check that the amount1 is greater than the idealAmount1 otherwise throw an error
    if (amount1 < idealAmount1) {
      throw new Error(
        `Cannot add liquidity in these amounts. You need to supply at least ${idealAmount1} ${
          pool["token-1"].split(".")[1]
        } along with ${amount0} ${pool["token-0"].split(".")[1]}`,
      );
    }
  }

  //define the txOptions for the addLiquidity function
  const txOptions = {
    contractAddress: AMM_CONTRACT_ADDRESS,
    contractName: AMM_CONTRACT_NAME,
    functionName: "add-liquidity",
    functionArgs: [
      principalCV(pool["token-0"]),
      principalCV(pool["token-1"]),
      uintCV(pool.fee),
      uintCV(amount0),
      uintCV(amount1),
      uintCV(0),
      uintCV(0),
    ],
  };
  return txOptions;
}

//removeLiquidity function for the transaction to the contract
export async function removeLiquidity(pool: Pool, liquidity: number) {
  //define the txOptions for the removeLiquidity function
  const txOptions = {
    contractAddress: AMM_CONTRACT_ADDRESS,
    contractName: AMM_CONTRACT_NAME,
    functionName: "remove-liquidity",
    functionArgs: [
      principalCV(pool["token-0"]),
      principalCV(pool["token-1"]),
      uintCV(pool.fee),
      uintCV(liquidity),
    ],
  };
  return txOptions;
}

//swap function for the transactions to the contract
export async function swap(pool: Pool, amount: number, zeroForOne: boolean) {
  //define the txOptions for the swap function
  const txOptions = {
    contractAddress: AMM_CONTRACT_ADDRESS,
    contractName: AMM_CONTRACT_NAME,
    functionName: "swap",
    functionArgs: [
      principalCV(pool["token-0"]),
      principalCV(pool["token-1"]),
      uintCV(pool.fee),
      uintCV(amount),
      boolCV(zeroForOne),
    ],
  };
  return txOptions;
}

//getUserLiquidity function for the transactions to the contract
export async function getUserLiquidity(pool: Pool, user: string) {
  //define the userLiquidity result
  const userLiquidityResult = await fetchCallReadOnlyFunction({
    contractAddress: AMM_CONTRACT_ADDRESS,
    contractName: AMM_CONTRACT_NAME,
    functionName: "get-position-liquidity",
    functionArgs: [bufferCV(Buffer.from(pool.id, "hex")), principalCV(user)],
    senderAddress: AMM_CONTRACT_ADDRESS,
    network: STACKS_TESTNET,
  });
  if (userLiquidityResult.type !== "ok") {
    return 0;
  }

  if (userLiquidityResult.value.type !== "uint") {
    return 0;
  }

  return parseInt(userLiquidityResult.value.value.toString());
}
