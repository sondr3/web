/**
 * Helper function for converting from undefined to dates... mostly because
 * this cannot be done nicely with the ternary operator due to bad type
 * inference.
 *
 * @param date - Date to convert
 * @returns The converted date, or undefined
 */
export const convertDate = (date: string | undefined): Date | undefined => {
  if (date === undefined) return date
  return new Date(date)
}
