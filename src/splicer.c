/*This code generates 1 second noise as a mono  16bit 44100Hz wav file*/
/* when called from the command line, it needs to have one argument:
the output filename, missing the file extension (this is added automatically)*/

/* make_wav.c
 * Creates a WAV file from an array of ints.
 * Output is monophonic, signed 16-bit samples
 * copyright
 * Fri Jun 18 16:36:23 PDT 2010 Kevin Karplus
 * Creative Commons license Attribution-NonCommercial
 *  http://creativecommons.org/licenses/by-nc/3.0/
 */

#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "make_wav.h"
#include <time.h>

void write_little_endian(unsigned int word, int num_bytes, FILE *wav_file)
{
    unsigned buf;
    while(num_bytes>0)
    {   buf = word & 0xff;
        fwrite(&buf, 1,1, wav_file);
        num_bytes--;
    word >>= 8;
    }
}


void write_wav(char * filename, unsigned long num_samples, short int * data, int s_rate)
{
    FILE* wav_file;
    unsigned int sample_rate;
    unsigned int num_channels;
    unsigned int bytes_per_sample;
    unsigned int byte_rate;
    unsigned long i;    /* counter for samples */

    num_channels = 1;   /* monoaural */
    bytes_per_sample = 2;

    if (s_rate<=0) sample_rate = 44100;
    else sample_rate = (unsigned int) s_rate;

    byte_rate = sample_rate*num_channels*bytes_per_sample;

    wav_file = fopen(filename, "wb");
    assert(wav_file);   /* make sure it opened */

    /* write RIFF header */
    fwrite("RIFF", 1, 4, wav_file);
    write_little_endian(36 + bytes_per_sample* num_samples*num_channels, 4, wav_file);
    fwrite("WAVE", 1, 4, wav_file);

    /* write fmt  subchunk */
    fwrite("fmt ", 1, 4, wav_file);
    write_little_endian(16, 4, wav_file);   /* SubChunk1Size is 16 */
    write_little_endian(1, 2, wav_file);    /* PCM is format 1 */
    write_little_endian(num_channels, 2, wav_file);
    write_little_endian(sample_rate, 4, wav_file);
    write_little_endian(byte_rate, 4, wav_file);
    write_little_endian(num_channels*bytes_per_sample, 2, wav_file);  /* block align */
    write_little_endian(8*bytes_per_sample, 2, wav_file);  /* bits/sample */

    /* write data subchunk */
    fwrite("data", 1, 4, wav_file);
    write_little_endian(bytes_per_sample* num_samples*num_channels, 4, wav_file);
    for (i=0; i< num_samples; i++)
    {   write_little_endian((unsigned int)(data[i]),bytes_per_sample, wav_file);
    }

    fclose(wav_file);
}


/* My code*/

#define S_RATE  (44100)
#define BUF_SIZE (S_RATE*1) /* x second buffer */
#define BitRate (65536)
#define HEADER_SIZE (22) // this is the header size (actually 44 bytes, but in 22 8 byte blocks)
#define FILE_SIZE (BUF_SIZE+HEADER_SIZE)


int rnd(int range);
void seedrnd(void);
int randshuffler(int pool, int total);

/*This main function takes four arguments when called via the command line:
- input file 1
- input file 2
- output file name
- random seed*/
int main(int argc, char* argv[])
{
     // This has been optimized for speed.
     int i,k,l,buff_find;
     int start_pos, end_pos, pos_diff, size, start_seek;
     char inname1[40], inname2[40], outname[40];
     short int bufferin1[FILE_SIZE],bufferin2[FILE_SIZE],newbuffer[BUF_SIZE];
     int pos,randseed,new_start_pos,new_end_pos,poss_pos,new_start_pos_header,new_end_pos_header,distance;

     sprintf(inname1, "%s",argv[1]);
     sprintf(inname2, "%s",argv[2]);
     sprintf(outname, "%s",argv[3]);

     FILE * infile1 = fopen(inname1, "rb");
     FILE * infile2 = fopen(inname2, "rb");
     FILE * outfile = fopen(outname, "wb");

     if(argc==1 || argc==2)
     {
          printf("Error: You need to specify 2 taget filenames!");
     }
     else
     {
          randseed = atoi(argv[4]);
          srand(randseed);
          // This is the part of the file removed from FILE 2
         start_pos = randshuffler(30,(BUF_SIZE-1));
         pos_diff = BUF_SIZE-start_pos;
         end_pos = start_pos + randshuffler(30,pos_diff);
         size = end_pos-start_pos;
         //This is the place at which the segment copied is pasted back in
         poss_pos = BUF_SIZE-size;
         new_start_pos = randshuffler(30,poss_pos); //new starting position
         new_end_pos = new_start_pos + size; // new end position
         new_start_pos_header = new_start_pos + HEADER_SIZE;
         new_end_pos_header = new_end_pos + HEADER_SIZE;
         distance = start_pos - new_start_pos;

         //COMMENT THESE LINES OUT WHEN RUNNING TO SPEED UP PROCESS:
        /* printf("FILE 2 Original Position\n");
         printf("Start Position: %i End Position: %i Size: %i Pos_diff: %i\n", start_pos,end_pos,size,pos_diff);
         printf("Target Position in FILE 1\n");
         printf("Start Position: %i End Position: %i Distance: %i \n", new_start_pos,new_end_pos,distance);
         printf("new_start_pos_header: %i new_end_pos_header: %i \n", new_start_pos_header,new_end_pos_header);
         */

         // Copy all of file 1
         fread(bufferin1, sizeof(short int), FILE_SIZE,infile1);

         // Copy all of file 2
         fread(bufferin2, sizeof(short int), FILE_SIZE,infile2);

         i = 1;

         for(k=HEADER_SIZE;k<FILE_SIZE;k++)
         {
              l = k-HEADER_SIZE;
              if (k<new_start_pos_header || k>new_end_pos_header)
              {
                    newbuffer[l] = (short int)(bufferin1[k]);
              }
              else
              {
                   buff_find = k+distance;
                   newbuffer[l] = (short int)(bufferin2[buff_find]);
              }
         }

         write_wav(outname, BUF_SIZE, newbuffer, S_RATE);
         printf("\nWrote: %s\n",outname);

          // cleanup
          if (infile1) { fclose(infile1); }
          if (infile2) { fclose(infile2); }
     }

     return(0);
}

/*creates an entropy pool to make the randomness more random*/
int randshuffler(int pool, int total)
{
     int z;
     int randbuff[pool];
     int randmid;
     int randout;

     for(z=0;z<pool;z++)
     {
          randmid = rnd(total);
          randbuff[z] = (int)(randmid);
     }
     randout = randbuff[rnd(pool)];
     return(randout);
}

/*generates random number in the range given
It is designed for  */
int rnd(int range)
{
     int r1;
     int r2;
     int rtotal;

     r1=rand();
     r2=rand();
     rtotal=(r1+r2)%range;
     return(rtotal);
}

/* seeds the random number using the time*/
void seedrnd(void)
{
     srand((unsigned)time(NULL));
}
