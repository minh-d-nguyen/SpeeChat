#!/usr/bin/env python3
# Requires PyAudio and PySpeech.
import os
import sys
import speech_recognition as sr

def get_speech():
    # Speech recognition using Google Speech Recognition
    try:
        line = ""
        # Record Audio
        r = sr.Recognizer()
        with sr.Microphone() as source:
            r.adjust_for_ambient_noise(source)
            audio = r.listen(source)
        # for testing purposes, we're just using the default API key
        # to use another API key, use `r.recognize_google(audio, key="GOOGLE_SPEECH_RECOGNITION_API_KEY")`
        # instead of `r.recognize_google(audio)`
        line = r.recognize_google(audio)
        print line
        return line
    except sr.UnknownValueError:
        return ""
    except sr.RequestError:
        return ""
