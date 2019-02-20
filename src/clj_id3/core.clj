(ns clj-id3.core
  (:gen-class)
  (:import [com.mpatric.mp3agic Mp3File ID3v2 ID3v24Tag]
           [java.io RandomAccessFile]
           [java.nio.file Files]
           [java.text Normalizer])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn normalize-str [str]
  (Normalizer/normalize str java.text.Normalizer$Form/NFKC))

(defn normalize-id3 [data]
  (let [{:keys [mp3 file
                track artist title album original-artist album-artist
                year genre genre-description original-artist album-artist album-image album-image-mime-type]} data]
    (if-not (and track title album (or artist album-artist))
      (println file " doesn't have enough tags.:" track artist title album album-artist)
      (let [id3 (ID3v24Tag.)
            data (cond-> data
                   artist (#(let [artist (normalize-str artist)]
                              (.setArtist id3 artist)
                              (assoc % :artist artist)))
                   title (#(let [title (normalize-str title)]
                             (.setTitle id3 title)
                             (assoc % :title title)))
                   album (#(let [album (normalize-str album)]
                             (.setAlbum id3 album)
                             (assoc % :album album)))
                   original-artist (#(let [original-artist (normalize-str original-artist)]
                                       (.setOriginalArtist id3 original-artist)
                                       (assoc % :original-artist original-artist)))
                   album-artist (#(let [album-artist (normalize-str album-artist)]
                                    (.setAlbumArtist id3 album-artist)
                                    (assoc % :album-artist album-artist))))]
        (when track (.setTrack id3 track))
        (when year (.setYear id3 year))
        (when genre (.setGenre id3 genre))
        (when genre-description (.setGenreDescription id3 genre-description))
        (when original-artist (.setOriginalArtist id3 original-artist))
        (when album-artist (.setAlbumArtist id3 album-artist))
        (when (and album-image album-image-mime-type)
          (.setAlbumImage id3 album-image album-image-mime-type))
        (.setId3v2Tag mp3 id3)
        data))))

(defn get-id3 [file]
  (try
    (let [mp3 (Mp3File. file)
          id3 (.getId3v2Tag mp3)]
      (if (.hasId3v2Tag mp3)
        (cond-> {}
          true (assoc :file file :mp3 mp3 :id3 id3)
          (.getTrack id3) (assoc :track (.getTrack id3))
          (.getArtist id3) (assoc :artist (.getArtist id3))
          (.getTitle id3) (assoc :title (.getTitle id3))
          (.getAlbum id3) (assoc :album (.getAlbum id3))
          (.getYear id3) (assoc :year (.getYear id3))
          (.getGenre id3) (assoc :genre (.getGenre id3))
          (.getGenreDescription id3) (assoc :genre-description (.getGenreDescription id3))
          (.getComment id3) (assoc :comment (.getComment id3))
          (.getLyrics id3) (assoc :lyrics (.getLyrics id3))
          (.getComposer id3) (assoc :composer (.getComposer id3))
          (.getPublisher id3) (assoc :publisher (.getPublisher id3))
          (.getOriginalArtist id3) (assoc :original-artist (.getOriginalArtist id3))
          (.getAlbumArtist id3) (assoc :album-artist (.getAlbumArtist id3))
          (.getCopyright id3) (assoc :copyright (.getCopyright id3))
          (.getUrl id3) (assoc :url (.getUrl id3))
          (.getEncoder id3) (assoc :encoder (.getEncoder id3))
          (.getAlbumImage id3) (assoc :album-image (.getAlbumImage id3))
          (.getAlbumImageMimeType id3) (assoc :album-image-mime-type (.getAlbumImageMimeType id3)))
        {:file file}))
    (catch Exception e
      (println file " throw erros.")
      (.printStackTrace e))))

(defn save-mp3 [data]
  (let [{:keys [mp3 id3 file
                track artist title album album-artist]} data
        out (io/file "output"
                     (or album-artist artist)
                     album
                     (format "%02d %s.mp3"
                             (-> track
                                 (str/split #"/")
                                 first
                                 Integer/parseInt) title))]
    (io/make-parents out)
    (try
      (when (.exists out) (println (.getAbsolutePath out) " exists."))
      (.save mp3 (.getAbsolutePath out))
      (catch Exception e
        (println file " failed to save.")
        (.printStackTrace e)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> (io/file "./input")
       (file-seq)
       (filter #(.isFile %))
       (map #(.getAbsolutePath %))
       (filter #(or (str/ends-with? % ".mp3")
                    (println % " is not music file.")))
       (map get-id3)
       (filter #(or (:id3 %)
                    (println % " doesn't have id3 tag.")))
       (map normalize-id3)
       (filter identity)
       (map save-mp3)
       (doall)))
