import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";
import Axios, { AxiosRequestConfig, AxiosResponse } from "axios";
import { v4 } from "uuid";
import { JsonRpcRequest, JsonRpcResponse } from "./jsonrpc";
import { LabeledTree } from "./labeled_tree";

const jsonrpcRequest = async <T>(method: JsonRpcRequest["method"], ...params: JsonRpcRequest["params"]): Promise<T> => {
  const config: AxiosRequestConfig = {
    headers: { "Content-Type": "application/json" },
  };
  const data: JsonRpcRequest = {
    jsonrpc: "2.0",
    id: v4(),
    method,
    params,
  };
  const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<T, unknown>>>(
    "./api/proxy",
    data,
    config
  );
  if ("error" in response.data) {
    throw Error(JSON.stringify(response.data.error));
  }
  return response.data.result;
};

export const constructDerivation = createAsyncThunk<
  { index: number; derivations: LabeledTree[] },
  { grammar: LabeledTree[]; index: number }
>("constructDerivation", async ({ grammar, index }) => {
  const derivations = await jsonrpcRequest<LabeledTree[]>("construct", grammar);
  return { index, derivations };
});

export const extractGrammar = createAsyncThunk<
  { index: number; grammars: LabeledTree[][] },
  { tree: LabeledTree; index: number }
>("extractGrammar", async ({ tree, index }, thunkAPI) => {
  const grammars = await jsonrpcRequest<LabeledTree[][]>("extract", tree);
  thunkAPI.dispatch(constructDerivation({ grammar: grammars[0], index }));
  return { index, grammars };
});

export const generateTrees = createAsyncThunk<LabeledTree[], string>("generateTrees", async (text, thunkAPI) => {
  const { sentences } = await jsonrpcRequest<{ sentences: { binaryParse: LabeledTree }[] }>("process", text, {
    annotators: "tokenize,ssplit,pos,lemma,ner,parse",
    "parse.binaryTrees": "true",
  });
  const trees = sentences.map((sentence) => sentence.binaryParse);
  for (let index = 0; index < trees.length; index++) {
    thunkAPI.dispatch(extractGrammar({ tree: trees[index], index }));
  }
  return trees;
});

export const initialState = {
  derivations: [] as LabeledTree[][],
  allGrammars: [] as LabeledTree[][][],
  grammars: [] as LabeledTree[][],
  trees: [] as LabeledTree[],
};

export const slice = createSlice({
  name: "appSlice",
  initialState,
  reducers: {},
  extraReducers: (builder) => {
    builder.addCase(generateTrees.fulfilled, (state, { payload }) => {
      state.trees = payload;
    });
    builder.addCase(extractGrammar.fulfilled, (state, { payload }) => {
      state.allGrammars[payload.index] = payload.grammars;
    });
    builder.addCase(constructDerivation.fulfilled, (state, { payload }) => {
      state.derivations[payload.index] = payload.derivations;
    });
  },
});

export type RootState = ReturnType<typeof slice.reducer>;
