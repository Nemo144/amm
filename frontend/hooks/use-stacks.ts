"use client";

import {
  addLiquidity,
  createPool,
  Pool,
  removeLiquidity,
  swap,
} from "@/lib/amm";

import {
  AppConfig,
  openContractCall,
  showConnect,
  type UserData,
  UserSession,
} from "@stacks/connect";

import { PostConditionMode } from "@stacks/transactions";
import { useState, useEffect } from "react";

//define the app details
const appDetails = {
  name: "Full Range Amm",
  icon: "https://cryptologos.cc/logos/stacks-stx-logo.png",
};

//the useStacks function
export function useStacks() {
  //define the state for the userData
  const [userData, setUserData] = useState<UserData | null>(null);

  //define the appConfig and the userSession
  const appConfig = new AppConfig(["store_write"]);
  const userSession = new UserSession({ appConfig });

  //define the connectWallet function
  function connectWallet() {
    showConnect({
      appDetails,
      onFinish: () => {
        window.location.reload();
      },
      userSession,
    });
  }

  //define the disconnectWallet function
  function disconnectWallet() {
    userSession.signUserOut();
    setUserData(null);
  }

  //define the handleCreatePool function
  async function handleCreatePool(token0: string, token1: string, fee: number) {
    console.log("handleCreatePool called", { token0, token1, fee });
    try {
      //check if the user is connected otherwise throw an error
      if (!userData) throw new Error("user not connected");

      //define the options variable for the createPool function
      const options = await createPool(token0, token1, fee);

      //define the OpenContractCall function
      await openContractCall({
        ...options,
        appDetails,
        onFinish: (data) => {
          window.alert("sent create Pool transaction");
          console.log(data);
        },
        postConditionMode: PostConditionMode.Allow,
      });
    } catch (_err) {
      const err = _err as Error;
      console.log(err);
      window.alert(err.message);
      return;
    }
  }

  //define the handleSwap function
  async function handleSwap(pool: Pool, amount: number, zeroForOne: boolean) {
    try {
      //check if the user is connected otherwise throw an error
      if (!userData) throw new Error("user not connected");

      //define the options variable for the swap function
      const options = await swap(pool, amount, zeroForOne);

      //define the OpenContractCall function
      await openContractCall({
        ...options,
        appDetails,
        onFinish: (data) => {
          window.alert("sent swap transaction");
          console.log(data);
        },
        postConditionMode: PostConditionMode.Allow,
      });
    } catch (_err) {
      const err = _err as Error;
      console.log(err);
      window.alert(err.message);
      return;
    }
  }

  //define the handleAddLiquidity function
  async function handleAddLiquidity(
    pool: Pool,
    amount0: number,
    amount1: number,
  ) {
    try {
      //check if user is connected otherwise throw an error
      if (!userData) throw new Error("user not connected");

      //define the options variable for the addLiquidity function
      const options = await addLiquidity(pool, amount0, amount1);

      //define the OpenContractCall function
      await openContractCall({
        ...options,
        appDetails,
        onFinish: (data) => {
          window.alert("sent the add liquidity transaction");
          console.log({ data });
        },
        postConditionMode: PostConditionMode.Allow,
      });
    } catch (_err) {
      const err = _err as Error;
      console.log(err);
      window.alert(err.message);
      return;
    }
  }

  //define the function for the handleRemoveLiquidity function
  async function handleRemoveLiquidity(pool: Pool, liquidity: number) {
    try {
      //check if user is connected otherwise throw an error
      if (!userData) throw new Error("user not connected");

      //define the options variable for the removeliquidity function
      const options = await removeLiquidity(pool, liquidity);

      //define the OpenContractCall function
      await openContractCall({
        ...options,
        appDetails,
        onFinish: (data) => {
          window.alert("sent remove liquidity transaction");
          console.log(data);
        },
      });
    } catch (_err) {
      const err = _err as Error;
      console.log(err);
      window.alert(err.message);
      return;
    }
  }

  //define the effect hook to handle the side effects of the userSessions
  useEffect(() => {
    //check if user is signed in and update the userData
    if (userSession.isSignInPending()) {
      userSession.handlePendingSignIn().then((userData) => {
        setUserData(userData);
      });
    } else if (userSession.isUserSignedIn()) {
      setUserData(userSession.loadUserData());
    }
  }, []);

  return {
    connectWallet,
    disconnectWallet,
    userData,
    handleCreatePool,
    handleAddLiquidity,
    handleSwap,
    handleRemoveLiquidity,
  };
}
