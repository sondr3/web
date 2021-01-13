/**
 * Like {@link Partial} but works on nested objects too.
 */
export type DeepPartial<T> = {
  [P in keyof T]?: DeepPartial<T[P]>
}

/**
 * Utility type for when reading something only returns the values as strings.
 */
export type PartialStringyTyped<T> = { [Key in keyof T]: string | undefined }
