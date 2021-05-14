import { User } from "../interfaces"
import { ListItem } from "./list-item"

type Props = {
  items: User[]
}

export const List = ({ items }: Props): JSX.Element => (
  <ul>
    {items.map((item) => (
      <li key={item.id}>
        <ListItem data={item} />
      </li>
    ))}
  </ul>
)
