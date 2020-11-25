import { LabeledTree } from "./labeled_tree";

export type RequestActions =
  | {
      type: "ConstructDerivationRequest";
      index: number;
      grammar: LabeledTree[];
    }
  | {
      type: "GenerateTreesRequest";
      text: string;
    }
  | {
    type: "ExtractGrammarRequest";
    index: number,
    tree: LabeledTree;
  };
export type ResponseActions =
  | {
    type: "ConstructDerivationResponse";
    index: number;
    derivations: LabeledTree[]
  }
  | {
    type: "ExtractGrammarResponse";
    index: number;
    grammars: LabeledTree[][]
  }
  | {
    type: "GenerateTreesResponse";
    trees: LabeledTree[]
  }
