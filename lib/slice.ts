import { createAsyncThunk } from '@reduxjs/toolkit'
import Axios, { AxiosRequestConfig, AxiosResponse } from "axios"
import { v4 } from "uuid"
import { JsonRpcRequest, JsonRpcResponse } from "./jsonrpc";
import { LabeledTree } from './labeled_tree';
import { produce } from 'immer'

const constructDerivation = createAsyncThunk<LabeledTree[][], { grammar: LabeledTree[], index: number }>(
  'constructDerivation',
  async ({ grammar, index }, thunkAPI) => {
    const config: AxiosRequestConfig = {
      headers: { "Content-Type": "application/json" },
    };
    const data: JsonRpcRequest = {
      jsonrpc: "2.0",
      id: v4(),
      method: "construct",
      params: [grammar],
    };
    const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<any, any>>>(
      "./api/proxy",
      data,
      config
    );
    if ("error" in response.data) {
      throw Error(JSON.stringify(response.data.error));
    }
    const derivations = response.data.result as LabeledTree[];
    return produce(thunkAPI.getState.derivations, (draft) => {
      draft[index] = derivations
    })
  }
)

const extractGrammar = createAsyncThunk<LabeledTree[][][], { tree: LabeledTree, index: number }>(
  'extractGrammar',
  async ({ tree, index }, thunkAPI) => {
    const config: AxiosRequestConfig = {
      headers: { "Content-Type": "application/json" },
    };
    const data: JsonRpcRequest = {
      jsonrpc: "2.0",
      id: v4(),
      method: "extract",
      params: [tree],
    };
    const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<any, any>>>(
      "./api/proxy",
      data,
      config
    );
    if ("error" in response.data) {
      throw Error(JSON.stringify(response.data.error));
    }
    const grammars = response.data.result as LabeledTree[][];
    thunkAPI.dispatch(constructDerivation({ grammar: grammars[0], index }));
    return produce(thunkAPI.getState.allGrammars, (draft) => {
      draft[index] = grammars
    })
  }
)

const generateTrees = createAsyncThunk<LabeledTree[], string>(
  'generateTrees',
  async (text, thunkAPI) => {
    const config: AxiosRequestConfig = {
      headers: {
        "Content-Type": "application/json",
      },
    };
    const data: JsonRpcRequest = {
      jsonrpc: "2.0",
      id: v4(),
      method: "process",
      params: [
        text,
        {
          annotators: "tokenize,ssplit,pos,lemma,ner,parse",
          "parse.binaryTrees": "true",
        },
      ],
    };
    const response = await Axios.post<JsonRpcRequest, AxiosResponse<JsonRpcResponse<any, any>>>(
      "./api/proxy",
      data,
      config
    );
    if ("error" in response.data) {
      throw Error(JSON.stringify(response.data.error));
    }
    const trees = response.data.result.sentences.map((s) => s.binaryParse) as LabeledTree[];
    for (let index = 0; index < trees.length; index++) {
      thunkAPI.dispatch(extractGrammar({ tree: trees[index], index }))
    }
    return trees;
  }
)
