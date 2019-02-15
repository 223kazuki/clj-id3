(ns clj-id3.core
  (:gen-class)
  (:import [com.mpatric.mp3agic Mp3File ID3v2]
           [java.io RandomAccessFile]
           [java.nio.file Files]
           [java.text Normalizer])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn normalize-str [str]
  (Normalizer/normalize str java.text.Normalizer$Form/NFKC))

(defn normalize-id3 [data]
  (let [{:keys [mp3 id3 file
                artist title album composer publisher original-artist album-artist]} data]
    (if-not (and artist title album album-artist)
      (println file " doesn't have enough tags.")
      (let [data (cond-> data
                   artist (#(let [artist (normalize-str artist)]
                              (.setArtist id3 artist)
                              (assoc % :artist artist)))
                   title (#(let [title (normalize-str title)]
                             (.setTitle id3 title)
                             (assoc % :title title)))
                   album (#(let [album (normalize-str album)]
                             (.setAlbum id3 album)
                             (assoc % :album album)))
                   composer (#(let [composer (normalize-str composer)]
                                (.setComposer id3 composer)
                                (assoc % :composer composer)))
                   publisher (#(let [publisher (normalize-str publisher)]
                                 (.setPublisher id3 publisher)
                                 (assoc % :publisher publisher)))
                   original-artist (#(let [original-artist (normalize-str original-artist)]
                                       (.setOriginalArtist id3 original-artist)
                                       (assoc % :original-artist original-artist)))
                   album-artist (#(let [album-artist (normalize-str album-artist)]
                                    (.setAlbumArtist id3 album-artist)
                                    (assoc % :album-artist album-artist))))]
        (.setId3v2Tag mp3 id3)
        data))))

(defn get-id3 [file]
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
      {:file file})))

(defn save-mp3 [data]
  (let [{:keys [mp3 id3 file
                track artist title album album-artist]} data
        ;; TODO: File path including "/"
        out (io/file "output"
                     (or album-artist artist)
                     album
                     (format "%02d %s.mp3"
                             (-> track
                                 (str/split #"/")
                                 first
                                 Integer/parseInt) title))]
    (println out " generated.")
    (io/make-parents out)
    (.save mp3 out)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> (io/file "input")
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
       (map save-mp3)))

(-main)