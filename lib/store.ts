/* eslint-disable @typescript-eslint/no-unused-vars */
import React from "react";
import { RequestActions } from "./actions";
import { LabeledTree } from "./labeled_tree";

export interface AppStore {
  text: string,
  trees: LabeledTree[],
  allGrammars: LabeledTree[][][],
  grammars: LabeledTree[][],
  derivations: LabeledTree[][],
  dispatch: (_: RequestActions) => void
}

export const defaultAppStore: AppStore = {
  text: "",
  trees: [],
  allGrammars: [],
  grammars: [],
  derivations: [],
  dispatch: (_: RequestActions) => {return;}
}

export const AppContext = React.createContext<AppStore>(defaultAppStore);
