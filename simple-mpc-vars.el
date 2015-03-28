(defgroup simple-mpc nil
  "simple-mpc - provides a simple interface to mpc"
  :group 'multimedia)

(defcustom simple-mpc-mpd-playlist-directory "~/.mpd/playlists/"
  "The directory `simple-mpc-load-playlist' will look for
playlists."
  :group 'simple-mpc
  :type 'directory)

(provide 'simple-mpc-vars)
