import type { NextApiRequest, NextApiResponse, NextApiHandler } from "next";
import axios from "axios";

const route = {
  process: "http://localhost:8081",
  extract: "http://localhost:8082",
  construct: "http://localhost:8082",
};

async function handler(
  req: NextApiRequest,
  res: NextApiResponse
): Promise<void> {
  const response = await axios.request({
    method: "POST",
    url: route[req.body.method],
    data: req.body,
    headers: req.headers,
  });
  res.status(200).json(response.data);
}

export default handler as NextApiHandler;
