from PIL import ImageGrab, ImageOps, ImageTk
import win32gui

toplist, winlist = [], []
def enum_cb(hwnd, results):
    txt = win32gui.GetWindowText(hwnd)
    if "emacs" in txt:
        print((hwnd, txt))
    winlist.append((hwnd, txt))
win32gui.EnumWindows(enum_cb, toplist)

from win32api import GetSystemMetrics
xx, yy = GetSystemMetrics(0), GetSystemMetrics(1)
yy = 1090
emacs = [(hwnd, title) for hwnd, title in winlist if 'emacs.exe' in title.lower()]
print(emacs)
hwnd = emacs[0][0]

win32gui.SetForegroundWindow(hwnd)
bbox = win32gui.GetWindowRect(hwnd)
top = 31
bbox = int(xx/2), top, xx, yy+top
img = ImageOps.mirror(ImageGrab.grab(bbox))

import tkinter as tk


image_window = tk.Tk()

img = ImageOps.mirror(ImageGrab.grab(bbox))
img = ImageTk.PhotoImage(img)
panel = tk.Label(image_window, image=img)
panel.pack(side="bottom", fill="both", expand="yes")

while True:
    image_window.update_idletasks()
    image_window.update()
    img2 = ImageOps.mirror(ImageGrab.grab(bbox))
    img2 = ImageTk.PhotoImage(img2)
    panel.configure(image=img2)
    panel.image = img2



#image_window.mainloop()
