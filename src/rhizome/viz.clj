(ns rhizome.viz
  (:use
    [rhizome.dot])
  (:require
    [clojure.string :as str]
    [clojure.java.shell :as sh]
    [clojure.java.io :as io])
  (:import
    [java.awt
     Toolkit]
    [java.awt.event
     KeyEvent]
    [javax.imageio
     ImageIO]
    [javax.swing
     AbstractAction JComponent JFrame JLabel JScrollPane ImageIcon KeyStroke]
    [javax.script
     ScriptEngineManager]))

(def ^:private shortcut-mask
  (.. Toolkit getDefaultToolkit getMenuShortcutKeyMask))

(def ^:private close-key
  (KeyStroke/getKeyStroke KeyEvent/VK_W shortcut-mask))

(defn create-frame
  "Creates a frame for viewing graphviz images.  Only useful if you don't want to use the default frame."
  [name]
  (delay
    (let [frame (JFrame. ^String name)
          image-icon (ImageIcon.)
          pane (-> image-icon JLabel. JScrollPane.)]
      (doto pane
        (.. (getInputMap JComponent/WHEN_IN_FOCUSED_WINDOW)
            (put close-key "closeWindow"))
        (.. getActionMap (put "closeWindow"
                              (proxy [AbstractAction] []
                                (actionPerformed [e]
                                  (.setVisible frame false))))))
      (doto frame
        (.setContentPane pane)
        (.setSize 1024 768)
        (.setDefaultCloseOperation javax.swing.WindowConstants/HIDE_ON_CLOSE))
      [frame image-icon])))

(defonce default-frame (create-frame "rhizome"))

(defn- send-to-front
  "Makes absolutely, completely sure that the frame is moved to the front."
  [^JFrame frame]
  (doto frame
    (.setExtendedState JFrame/NORMAL)
    (.setAlwaysOnTop true)
    .repaint
    .toFront
    .requestFocus
    (.setAlwaysOnTop false))

  ;; may I one day be forgiven
  (when-let [applescript (.getEngineByName (ScriptEngineManager.) "AppleScript")]
    (try
      (.eval applescript "tell me to activate")
      (catch Throwable e
        ))))

(defn view-image
  "Takes an `image`, and displays it in a window.  If `frame` is not specified, then the default frame will be used."
  ([image]
     (view-image default-frame image))
  ([frame image]
     (let [[^JFrame frame ^ImageIcon image-icon] @frame]
       (.setImage image-icon image)
       (.setVisible frame true)
       (java.awt.EventQueue/invokeLater
         #(send-to-front frame)))))

(defn- format-error [s err]
  (apply str
    err "\n"
    (interleave
      (map
        (fn [idx s]
          (format "%3d: %s" idx s))
        (range)
        (str/split-lines s))
      (repeat "\n"))))

(defn dot->image
  "Takes a string containing a GraphViz dot file, and renders it to an image.
  Requires GraphViz's layout commands on the shell's path. By default the 'dot'
  layout command is used, unless a another one is specified via the :cmd key.
  Available layout commands are listed at:
  http://www.graphviz.org/Documentation.php"
  [s & {:keys [cmd] :or {cmd "dot"}}]
  (let [{:keys [out err]} (sh/sh (name cmd) "-Tpng" :in s :out-enc :bytes)]
    (or
      (ImageIO/read (io/input-stream out))
      (throw (IllegalArgumentException. ^String (format-error s err))))))

(defn dot->svg
  "Takes a string containing a GraphViz dot file, and returns a string containing SVG.
  Requires GraphViz's layout commands on the shell's path. By default the 'dot'
  layout command is used, unless a another one is specified via the :cmd key.
  Available layout commands are listed at:
  http://www.graphviz.org/Documentation.php"
  [s & {:keys [cmd] :or {cmd "dot"}}]
  (let [{:keys [out err]} (sh/sh (name cmd) "-Tsvg" :in s)]
    (or
      out
      (throw (IllegalArgumentException. ^String (format-error s err))))))

(defn save-image
  "Saves the given image buffer to the given filename. The default
file type for the image is png, but an optional type may be supplied
as a third argument."
  ([image filename] 
     (save-image image "png" filename))
  ([image filetype filename] 
     (ImageIO/write image filetype (io/file filename))))

(defn graph->image
  "Takes a graph descriptor in the style of `graph->dot`, and returns a rendered image.
  Requires GraphViz's layout commands on the shell's path. By default the 'dot'
  layout command is used, unless a another one is specified via the :cmd key.
  Available layout commands are listed at:
  http://www.graphviz.org/Documentation.php"
  {:arglists (-> #'graph->dot meta :arglists)}
  [nodes adjacent & args]
  (let [dot (apply graph->dot nodes adjacent args)]
    (apply dot->image dot args)))

(defn graph->svg
  "Takes a graph descriptor in the style of `graph->dot`, and returns SVG.
  Requires GraphViz's layout commands on the shell's path. By default the 'dot'
  layout command is used, unless a another one is specified via the :cmd key.
  Available layout commands are listed at:
  http://www.graphviz.org/Documentation.php"
  {:arglists (-> #'graph->dot meta :arglists)}
  [nodes adjacent & args]
  (let [dot (apply graph->dot nodes adjacent args)]
    (apply dot->svg dot args)))

(def
  ^{:doc "Takes a graph descriptor in the style of `graph->dot`, and displays a rendered image.
  Requires GraphViz's 'dot' (or a specified algorithm using the :cmd key) on
  the shell's path. Possible algorithms include :dot, :neato, :fdp, :sfdp,
  :twopi, and :circo"
    :arglists (-> #'graph->dot meta :arglists)}
  view-graph
  (comp view-image graph->image))

(defn save-graph
  "Takes a graph descriptor in the style of `graph->dot`, and saves the image to disk
  using the filename provider under the :filename key.
  Requires GraphViz's layout commands on the shell's path. By default the 'dot'
  layout command is used, unless a another one is specified via the :cmd key.
  Available layout commands are listed at:
  http://www.graphviz.org/Documentation.php"
  {:arglists (-> #'graph->dot meta :arglists)}
  [nodes adjacent & {:keys [filename] :as options}]
  (let [args (apply concat options)
        dot (apply graph->dot nodes adjacent args)
        img (apply dot->image dot args)]
    (save-image img filename)))

(defn tree->image
  "Takes a tree descriptor in the style of `tree->dot`, and returns a rendered image.
  Requires GraphViz's layout commands on the shell's path. By default the 'dot'
  layout command is used, unless a another one is specified via the :cmd key.
  Available layout commands are listed at:
  http://www.graphviz.org/Documentation.php"
  {:arglists (-> #'tree->dot meta :arglists)}
  [branch? children root & args]
  (let [dot (apply tree->dot branch? children root args)]
    (apply dot->image dot args)))

(def
  ^{:doc "Takes a tree descriptor in the style of `tree->dot`, and displays a rendered image.
  Requires GraphViz's 'dot' (or a specified algorithm using the :cmd key) on
  the shell's path. Possible algorithms include :dot, :neato, :fdp, :sfdp,
  :twopi, and :circo"
    :arglists (-> #'tree->dot meta :arglists)}
  view-tree
  (comp view-image tree->image))

(defn save-tree
  "Takes a tree descriptor in the style of `tree->dot`, and saves the image to disk
  using the filename provider under the :filename key.
  Requires GraphViz's layout commands on the shell's path. By default the 'dot'
  layout command is used, unless a another one is specified via the :cmd key.
  Available layout commands are listed at:
  http://www.graphviz.org/Documentation.php"
  {:arglists (-> #'tree->dot meta :arglists)}
  [branch? children root & {:keys [filename] :as options}]
  (let [args (apply concat options)
        dot (apply tree->dot branch? children root args)
        img (apply dot->image dot args)]
    (save-image img filename)))
