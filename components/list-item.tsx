import Link from "next/link"

import { User } from "../interfaces"

type Props = {
  data: User
}

export const ListItem = ({ data }: Props): JSX.Element => (
  <Link href="/users/[id]" as={`/users/${data.id}`}>
    <a>
      {data.id}: {data.name}
    </a>
  </Link>
)
