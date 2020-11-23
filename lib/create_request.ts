import Axios from "axios";
import { v4 as uuidv4 } from "uuid";

export default async function request(method: string, ...params: unknown[]): Promise<unknown> {
  const response = await Axios.request(
    {
      method: "POST",
      url: "./api/proxy",
      data: {
        jsonrpc: "2.0",
        id: uuidv4(),
        method,
        params,
      },
    }
  );
  return response.data.result
}

