import { APIGatewayEvent, Context, Callback } from "aws-lambda";

async function handler(_event: APIGatewayEvent, _context: Context, callback: Callback): Promise<void> {
  await new Promise((res) => setTimeout(res, 500));
  callback(undefined, {
    statusCode: 200,
    body: JSON.stringify("Hello, world..."),
  });
}

export { handler };
