#! /bin/env bash

# Example setup script... which randomly happens to fit exactly the needs this project was created for.

cd data
git clone https://github.com/bridgetkromhout/devops-against-humanity.git
git pull
tail -n +2 "devops-against-humanity/first-printing-cards-DevOpsAgainstHumanity.csv" > data.csv

# Feeling adventurous? Use cards-DevOpsAgainstHumanity.csv instead!

cd ../latex

base64 -d <(echo \
JVBERi0xLjUKJbXtrvsKMyAwIG9iago8PCAvTGVuZ3RoIDQgMCBSCiAgIC9GaWx0ZXIgL0ZsYXRl\
RGVjb2RlCj4+CnN0cmVhbQp4nG2Zy84suXGE9/UU9QIuF+/kEwgQoIWlpeGF0brB6LOQtNDrO75I\
VvcZaTAY/IdsVjGvkZFZfzvuk//+/pfzP//3Pv/yj+O+Vl9nSvWqNZ0/zmedrzLWma9aplbpuls6\
69U+/36daV3r++Nxvs+cPz/ncpU8/Z6cy3cZ17w+G3lcTcff34119bwOvetKOplKujK37Mv28hWy\
7JWe/sXhrYNeNcbxWcVNr/OvR7rGLZmaJf3BVXmNZ/1+1ukueyPOf9d/Pf58FF3Z8jnHlWfTS2q9\
SmvP+v2sPy/t9Vp1/dv6+Dww89XW+S/L7537/L9vyMLzTufx3cnSOHeWY0nsfeXQg3l8lq9z9mut\
/tl4741DZvwIcV99Vm0s3zFk9CQf1/uaJZ9dx7hgL+UT+a59fj8eG+AWh89epnENxcTrs/HRaRs1\
3feOCiy97kuuPEe3b3+ce9310jXPNR1grV35HrYGr9pLBWm5rUtsEKSp6NbVPk+UdlVZq5WrjvJd\
xiWvz0btV6/Nz3tD2qVr1gjJJkE+y7hwL1/nUmD38qzfzzrPayrB3uchDWbqT2SkHBo8y9KvuT5x\
JHkkSLm/v7d+3Xc9FetVOfgsq5Sz+nst7VIqSL83FBxFl6at5yj7tmbhj73WC1K/RqvPAb0gFRv8\
eSLddua/LueVlp4/PjszWcv3d6NdbQZg1DYc+LrnuW8v0UD2KO14dt7PjqMES75tk5uwqduE4bLP\
Wjf+tD5wSuj5nNghpd1bIu3VJGGMF38+Um+XfvlARlKu1FS/6fxsKEXWxM1SXzHcyGPrtRT0VTmh\
t7Yub98KZ8XbXgoQE7n2+TnJOrl8lwKv2VnarXst6975Wsas/cCjUQZ45/qu76mD9bMGvteVU/pa\
BYmnsk/xyJ2PRsTrcCbo9jR/hsUwyi9xMQup755PxS2bim2h+n/onr//6dSvFSwdZ5fYLjfPxmhX\
mcJgICFXgqNIlCxEQZaVN8LIlEKidoTzb+Gp3kRkf/QcCqc5f9Izj8igzwlFogNPptO+Kg2RSJiA\
OtO3TF0rQ4MtQ0L5+u7wEPxl/QUqFCZDMfR6NgI4N5Bx2V1/ZedbZVY3Dv200a4++qkctFzsyLLr\
FMCMNi3VkGWFHIiJ2F2+z91Z8LJiKp0ymkC0h95CnCOXj2WK1vlJbUx3E1z79zAt7yOQHtOXwMXX\
4xrhnrJJcfB4TudG+S4Fy+QaaZMFLoCMtlBUzt4bLUUyZYOE3qtY1LI5JorUKzZrUznRD5RaoDIr\
3NBUABv6ZJe5vcTRU6KOz++Eq/SR9bgzK5wFQAcKBTBnORBNFakOPRmb6xcJ9fhYsgVd2esZWB+u\
abesodcmP7BRbTnNstDKiJJFQewbHa+FEOo5WdQpcFhjq1LwxPGsX9aVUvsckClIAF06EbUZ73TH\
xg5ZUgCAdLnmx8zHU/I/fgiDBaIV+c2sp2xIK9K8jU98FMoawUBFLwQpSa37HWslzasXVzUhpoya\
gNxM2aQ4PEt5PjtFPhttE7ZnY0QS+H1JKURQyzu6EGVJmJdFKbNswB+WVBj/yWhFtdRfP+V8aYOg\
PL5HuBj2o1dAesoAbcUeqrFBT0iQpPo+p5JaKYeDRjMBIoeetR6LbHg2+pe66KaSfmVDN6q+fTaO\
k7jvVevuhFSMURmL4naJbBWA2lo3p3MJMxTyq9Qnurf7jm/4F+WvTbcPtGWjlDsUInFyVI/SVWMn\
LlZJClvct4wF8Ax0z7aNBHF0YRq9jceH9VBYQeKh14Kl0heVTUFsJ9nSxHJuJkxcF0CpwFrCDTG+\
q/b0ibNfxiHB+fhzB/8P6+7KNHZoCqJkMTmP2ESt25kHj9B9kmQqSY8ZRI/gLTKpPOan5YY+sl3p\
bGBjJm9sz+lf9CLPxvHdyeZePx1Zl+6XilN/O9FLSS2wK8j8MqKFCSTCgcg9TCCLf/T5pb6Rn9Gn\
/f43p1Iu117H+c8jnb/V//93/Pf/qKm5zz8e9fydRElRrxXz5W7O5Dt4+HAPQe0pJhtZpWOV+is7\
RfRAwSOvybZ1SSNoGbWv32YepHDe5Ud+9Inl8C3iImSyf1cBsPn7gGUVkZyAjWkAKiq58gfklLi7\
BR7Sq4ic8utGSWK9NTtYMEuoUqPvYiLxNnRieVz+dk2ku8upumJHFYXEoTxIrJ1aghLIQAJdF1bF\
K8tM+atxMOHDkVzIMKeep8xJKuUpBzlQb0slFW2tJNrybMF3hEEigNSBrKCDXytXhlwrUAw6pp9x\
TVA4ExfFjNo0qSh7ii6IhhXdX72+eYV4GuHJIwIn5dEdcMgbzEewci1hg4UPtdHW5pQ3TegtF9F8\
6i+y6fda2W4OU28v13gQVvGc6eJHhRPqSkpIao4XOKKXc/fxiA9uIi0tYprZLucERDjRts10BGXt\
qAFtNMNAXrcww6DKpanQT8pP0v+uTmNanrLlVV/ZlqSq2WU4Ad8MDQqdvu5emybSNQGxadZoKBXm\
HmTQ4tISd5iWXiS4J3b1BA6SeFaQtjBHLU0rO6sS0U6vKta2XNKZZawRapj8SWNxSmVQDVerai3W\
UocUi9YN5IHpREDphCK5TbwSliLW4WXLD0g8WHCHdVTLRt3jAYi4fLMA0jYdei9nSfeBQf4cZv0O\
kRZZndW8BeeM8OjW3u/PdmEhvnoUrpwp7koQ9fm1r8OpEBQCLpnwkayGCUYNSsgQRwXaTGxWP5Ai\
AiFjDhN+Cc4dQS5HzWl2RoS+7EQbSTvDWIUVnFLsLLL+Dwfv5QaMhoIMce6YRowSLJgpyHTCmV0q\
/Nj2z91pnchQYlxHTJMEYMYJcpZ6FR0ZOQ7evXYaKQZggfd0HtGr0hg5LkkoyyAdAjruZoqdxA7r\
6o5pYIqoaqMGrNGbUMpq2S4UMB5uGMGHfAdLBgYwOX85yt8R+fS0KUoP98UruA6XZVpObgOlLK9D\
F3H8hCBmzLBoSqbBWIOw/mH9V5iDzOAw+J5GcGBiKztug0aH7NOy97XV3mm3H0EyEzlAsjm11x5S\
DZm2FadXwEhionf4ETejxcT55SQgokjtDlzObtrjKJnJ+0pBK73cvxuSyAbnMtYWtI/mfXurG2+S\
uF6NPFUCAFVJ1Z2gTbDvjpBzD8jIuDEtJbWIKUVkAJoDQUktjtv+Xkz0wbR1d49fWll7ruEGIlHs\
OKIyCIxzBNYALAJqPBEw2NyacDlJxtIGUPZGMeoB3uBraYG0JYCDRl2NJNIZ1FRVmZ2x9txUv5vV\
UuJ6zJGWGdfbviMbePtUeXQFy8nXRp1dMHet8w5hDMnEADNKwCPshnN73c69I5Rl+D4D6EbgdDd3\
cNIPZwyv5dm3g0mvPSxtCtkWFMTqGLNJjRJq0nyREDTUyWiFCQnzsblRMZb+CGSwcYYJ4dugVJhz\
WoEoTHP7MvKgeAQhOcDfl1PBOdm7p9SsqZqYcz8R3OC2UZGDR8Ooe6AU0I7dyQzpdu8TgxGAoIlW\
GEs6Iov1hvJWz7OkAxZAcpfrFqxDZZDJda2RsYRSBQBa8T+yNMJAFBzKgMQsMjCZRUs7mhH4xaTD\
ADOCcJCjdKF4/o5pbU9a71HXy6OVEqOVGnILGSo0bcajHrUgp7zMox7I4brngJLcpU2/u2VSuQDu\
eR9B+EJk5kRUGpp8BaNrLzVUAV5GjI/wx27XPME5XMtuDw3SiAMkgjofl7O+DPCeTHvGggmEbXU5\
izMQoXMx49hVLHOedpJyRsE27yczPTeStMwOFMKMDMKeqE9BA6iFM8faBU6GJR9WzKPUFe3ueZYY\
sVAScFfMRVhSr4EaT6kqzj5iDu8yrJ1q4eEEDCAVCzrtYgc0L0g3MJOH2Th5rYgkwqDqgs/j7DGn\
pmHYLOZue8zSXA8I1RAIul9WDInDH+w0j4wNdSz9yUEPUNMKDzaEGLt3pSZ7Tj0CDNnAlK4xXinP\
jtPcvHpqwF8/j2WKij4cehg6WVHrZ/ZgR6vmNmM1wxANTCO40j0MYbzexSjFJISOhiALdkhrow2i\
jRk9YVukmIEtE94pxiApyhRUiHbY6JL9yLSG0+SKNeWW9YiizKhieNgx4kOFXECjTIMbn348Nivi\
lwlIuLPfxJzgp7TjwIhqS9IthjMlfI3nSq1e7yfEXT0Z0DsnCCVp4sQdAEZrXGNUsXogDVOL7B2X\
eLVCdgVNdgmmPYp76hpq5eU6XKCKKT4G+QMfG1C+xHgvV693hDE0nd2m6Ea7YWLmCRV31sChojZA\
VNFS1Zhlqho7k/kmkCIGehBFppc91ra/uHi1oHN/UHRb4gGhnszmmeC3k4GLchQW/F34wEXfod+b\
Rzfr4QEUMJRBBTdQcPXcfI8/pbU9e8nVpfkV1RHt+PIZok9/vcEJqUfTkqijnPCHSXouvzMAICgO\
MeE5HYWIClvjFvo0k7oybeM5S1SMZEwr2dmBap4TKUsGXwVQzZO8eUWJ5mvfzUbfLIPvqY2YD8Lo\
YocxUn0sSvfFoEs54Ho6I5nJEojjisk3D0hxC7XolZxX967fzsvk7sUseRg1iKRXcA9wwQBk4BEy\
2O5FOU0hhb077zzgaMEFNlYNj7fsZKSEq4+HAhkAPS+mauexAXKZutHu0AVJWlD/bSLI254xcQgS\
IF4dq+8jTOpD2aND2IJrGDXP6xhr0itRxF4mKZ7Q77pDqAh3DldiM9gekwA+Fm9eTwdN2XQnw4R4\
eTYN7+A4iJ2X+05oCIHjIXryRwd/2Zr+fADW+XsmrUoOn7RhD9ka+jk+luCVXIiDOzrwFNMj0146\
I8/gJYGnmhB9n5DL8ohmzl+a9ncWaPUPP+ev5ZhqfUk99TUyoMashv34zstHnJ6jJhuTn8G8W/5q\
WxSvl7sktP+v4/8BXXJeGAplbmRzdHJlYW0KZW5kb2JqCjQgMCBvYmoKICAgMzY0NwplbmRvYmoK\
MiAwIG9iago8PAogICAvRXh0R1N0YXRlIDw8CiAgICAgIC9hMCA8PCAvQ0EgMSAvY2EgMSA+Pgog\
ICA+Pgo+PgplbmRvYmoKNSAwIG9iago8PCAvVHlwZSAvUGFnZQogICAvUGFyZW50IDEgMCBSCiAg\
IC9NZWRpYUJveCBbIDAgMCAzODAgMzgwIF0KICAgL0NvbnRlbnRzIDMgMCBSCiAgIC9Hcm91cCA8\
PAogICAgICAvVHlwZSAvR3JvdXAKICAgICAgL1MgL1RyYW5zcGFyZW5jeQogICAgICAvSSB0cnVl\
CiAgICAgIC9DUyAvRGV2aWNlUkdCCiAgID4+CiAgIC9SZXNvdXJjZXMgMiAwIFIKPj4KZW5kb2Jq\
CjEgMCBvYmoKPDwgL1R5cGUgL1BhZ2VzCiAgIC9LaWRzIFsgNSAwIFIgXQogICAvQ291bnQgMQo+\
PgplbmRvYmoKNiAwIG9iago8PCAvQ3JlYXRvciAoY2Fpcm8gMS4xMi4xNiAoaHR0cDovL2NhaXJv\
Z3JhcGhpY3Mub3JnKSkKICAgL1Byb2R1Y2VyIChjYWlybyAxLjEyLjE2IChodHRwOi8vY2Fpcm9n\
cmFwaGljcy5vcmcpKQo+PgplbmRvYmoKNyAwIG9iago8PCAvVHlwZSAvQ2F0YWxvZwogICAvUGFn\
ZXMgMSAwIFIKPj4KZW5kb2JqCnhyZWYKMCA4CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwNDA0\
OCAwMDAwMCBuIAowMDAwMDAzNzYyIDAwMDAwIG4gCjAwMDAwMDAwMTUgMDAwMDAgbiAKMDAwMDAw\
MzczOSAwMDAwMCBuIAowMDAwMDAzODM0IDAwMDAwIG4gCjAwMDAwMDQxMTMgMDAwMDAgbiAKMDAw\
MDAwNDI0MiAwMDAwMCBuIAp0cmFpbGVyCjw8IC9TaXplIDgKICAgL1Jvb3QgNyAwIFIKICAgL0lu\
Zm8gNiAwIFIKPj4Kc3RhcnR4cmVmCjQyOTQKJSVFT0YK) >littleimage.pdf

cd ..

# Lets see if it works:
runhaskell texify.hs data/data.csv
# That should automatically run pdflatex.
# Now that I write these lines it seems like a silly idea.