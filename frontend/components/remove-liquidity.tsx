"use client";

import { useStacks } from "@/hooks/use-stacks";
import { getUserLiquidity, Pool } from "@/lib/amm";
import { useState, useEffect } from "react";
import { useStacksContext } from "@/context/stacks-context";

export interface RemoveLiquidityProps {
  pools: Pool[];
}

export const RemoveLiquidity = ({ pools }: RemoveLiquidityProps) => {
  //define the useStacks contexts for use in the removeLiquidity component
  const { userData, handleRemoveLiquidity } = useStacksContext();

  //define the state for the selected pool
  const [selectedPool, setSelectedPool] = useState<Pool>(pools[0]);

  //define the liquidity state
  const [liquidity, setLiquidity] = useState(0);

  //define the user total liquidity state
  const [userTotalLiquidity, setUserTotalLiquidity] = useState(0);

  //define the fetchUserLiquidity function
  async function fetchUserLiquidity() {
    //define the stx address
    const stxAddress = userData?.profile.stxAddress.testnet;

    if (!stxAddress) return;

    //update the getUserLiquidity function
    getUserLiquidity(selectedPool, stxAddress).then((liquidity) => {
      setUserTotalLiquidity(liquidity);
    });
  }

  //define the effect getUserLiquidity function
  useEffect(() => {
    getUserLiquidity;
  }, [selectedPool, userData]);

  return (
    <div className="flex flex-col max-w-md w-full gap-4 p-6 border border-gray-500 rounded-md">
      <h1 className="text-xl font-bold">Remove Liquidity</h1>
      <div className="flex flex-col gap-1">
        <span className="font-bold">Pool ID</span>
        <select
          className="border-2 border-gray-500 rounded-lg px-4 py-2 text-black"
          onChange={(e) => {
            const poolId = e.target.value;
            setSelectedPool(pools.find((pool) => pool.id === poolId)!);
          }}
        >
          {pools.map((pool) => (
            <option key={pool.id} value={pool.id}>
              {pool.id}
            </option>
          ))}
        </select>
      </div>
      <div className="flex flex-col gap-1">
        <div className="flex items-center justify-between">
          <span className="font-bold">Liquidity</span>
          <span>Max: {userTotalLiquidity}</span>
        </div>
        <input
          type="text"
          className="border-2 border-gray-500 rounded-lg px-4 py-2 text-black"
          value={liquidity}
          onChange={(e) => setLiquidity(parseInt(e.target.value))}
        />
      </div>

      <div className="flex flex-col gap-1">
        <span>
          Withdraw {selectedPool["token-0"].split(".")[1]}:{" "}
          {(liquidity / selectedPool.liquidity) * selectedPool["balance-0"]}
        </span>
        <span>
          Withdraw {selectedPool["token-1"].split(".")[1]}:{" "}
          {(liquidity / selectedPool.liquidity) * selectedPool["balance-1"]}
        </span>
      </div>

      <button
        className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded disabled:bg-gray-700 disabled:cursor-not-allowed"
        disabled={liquidity > userTotalLiquidity}
        onClick={() => handleRemoveLiquidity(selectedPool, liquidity)}
      >
        Remove Liquidity
      </button>
    </div>
  );
};
