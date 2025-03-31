echo ${USERID}
echo ${USER}
usermod -u ${USERID} ${USER}
groupmod -g ${GROUPID} ${USER}

cd /home/WAM
