"use client";

import { useStacks } from "@/hooks/use-stacks";
import { createContext, useContext } from "react";

//define the stackscontext
const StacksContext = createContext<ReturnType<typeof useStacks> | null>(null);

export const StacksProvider = ({ children }: { children: React.ReactNode }) => {
  const stacks = useStacks();

  return (
    <StacksContext.Provider value={stacks}>{children}</StacksContext.Provider>
  );
};

export const useStacksContext = () => {
  //define the context
  const context = useContext(StacksContext);

  if (!context)
    throw new Error("useStacksContext must be used within StacksProvider");
  return context;
};
