import { NextApiRequest, NextApiResponse } from "next"

import { sampleUserData } from "../../../utils/sample-data"

const handler = (_req: NextApiRequest, res: NextApiResponse): void => {
  try {
    if (!Array.isArray(sampleUserData)) {
      throw new TypeError("Cannot find user data")
    }

    res.status(200).json(sampleUserData)
  } catch (error) {
    const err = error as Error
    res.status(500).json({ statusCode: 500, message: err.message })
  }
}

export default handler
