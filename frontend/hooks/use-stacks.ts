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
}
