#!/usr/bin/env python3
# Requires PyAudio and PySpeech.
import os
import sys
import speech_recognition as sr

def get_speech():
    # Speech recognition using Google Speech Recognition
    try:
        # Record Audio
        r = sr.Recognizer()
        with sr.Microphone() as source:
            print("calibrating...")
            r.adjust_for_ambient_noise(source)
            print("Say something!")
            audio = r.listen(source)
        # for testing purposes, we're just using the default API key
        # to use another API key, use `r.recognize_google(audio, key="GOOGLE_SPEECH_RECOGNITION_API_KEY")`
        # instead of `r.recognize_google(audio)`
        print("recognizing...")
        command = r.recognize_google(audio)
        return command
    except sr.UnknownValueError:
        print("Could not understand audio")
    except sr.RequestError as e:
        print("Could not request results from Google Speech Recognition service; {0}".format(e))

