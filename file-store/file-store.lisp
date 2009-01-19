(in-package :cl-torrent.file-store)

(defstruct piece-spec
  size bytes-downloaded ranges hash)

(defstruct file-spec
  index size path pieces)

