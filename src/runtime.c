#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void runtime_castcheck(char* to, char** h, int hlen)
{
   int i = 0;
   while(i < hlen)
   {
     if(strcmp(to,h[i]) == 0) return;
     i= i +1;
   }
   printf("ERROR! Impossibile to cast %s to %s\n",h[0],to);
   abort();
}

