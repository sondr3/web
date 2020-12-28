export const convertDate = (date: string | undefined): Date | undefined => {
  if (date === undefined) return date
  return new Date(date)
}
