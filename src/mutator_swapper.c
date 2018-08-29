/*mutator_swapper.exe input_file_name_1 input_file_name_2 output_file_name_1 output_file_name_2 random_seed minimum_percentage_of_file_to_copy maximum_percentage_of_file_to_copy

This takes in two input files and swaps a section between size minimum_percentage_of_file_to_copy % and maximum_percentage_of_file_to_copy %.
This creates two new files which are output.

This main function takes four arguments when called via the command line:
- [1] input file 1  
- [2] input file 2
- [3] output file 1 name
- [4] output file 2 name
- [5] random seed
- [6] minimum percentage of file to copy (int)
- [7] maxmum percentage of file to copy (int)*/


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


int main(int argc, char* argv[])
{
     if(argc!=8) { printf("Error: Wrong number of variables (%i)", argc); }
     else
     {
        srand(atoi(argv[5])); // set random seed

        char input_filename1[40], input_filename2[40], output_filename1[40], output_filename2[40];
        short int input_file1_buffer[FILE_SIZE],input_file2_buffer[FILE_SIZE],output_file_buffer1[BUF_SIZE],output_file_buffer2[BUF_SIZE];
         float minimum_size_in_samples, maximum_size_in_samples;
        int length_in_samples, section_size_samples, start_position, end_position,buffer_position;

        //Input file 1
        sprintf(input_filename1, "%s",argv[1]);
         FILE * input_file1 = fopen(input_filename1, "rb");
        fread(input_file1_buffer, sizeof(short int), FILE_SIZE,input_file1);

        // Input file 2
         sprintf(input_filename2, "%s",argv[2]);
        FILE * input_file2 = fopen(input_filename2, "rb");
         fread(input_file2_buffer, sizeof(short int), FILE_SIZE,input_file2);

        //Ouput file 1
         sprintf(output_filename1, "%s",argv[3]);
         FILE * output_file1 = fopen(output_filename1, "wb");

         //Output file 2
         sprintf(output_filename2, "%s",argv[4]);
         FILE * output_file2 = fopen(output_filename2, "wb");


         // This calculates the size of the section that will be swapped in percentages
        minimum_size_in_samples = BUF_SIZE*(atof(argv[6])/100);
        maximum_size_in_samples = BUF_SIZE*(atof(argv[7])/100);
        length_in_samples = (int)(maximum_size_in_samples-minimum_size_in_samples);
        // for debug: printf("minimum_size_in_samples: %f maximum_size_in_samples: %f \n",minimum_size_in_samples,maximum_size_in_samples);
         
         // This calculates the size of the section that will be swapped in samples
        section_size_samples = minimum_size_in_samples + randshuffler(30,length_in_samples);
        start_position = randshuffler(30,(BUF_SIZE - section_size_samples));
        end_position = start_position + section_size_samples;

         //  Build the buffers for the output files
         for(buffer_position = HEADER_SIZE; buffer_position < FILE_SIZE; buffer_position++)
         {
              if (buffer_position < start_position || buffer_position > end_position)
              {
                    output_file_buffer1[buffer_position] = (short int)(input_file1_buffer[buffer_position + HEADER_SIZE]);
                    output_file_buffer2[buffer_position] = (short int)(input_file2_buffer[buffer_position + HEADER_SIZE]);
              }
              else
              {
                   output_file_buffer1[buffer_position] = (short int)(input_file2_buffer[buffer_position + HEADER_SIZE]);
                   output_file_buffer2[buffer_position] = (short int)(input_file1_buffer[buffer_position + HEADER_SIZE]);
              }
         }

         write_wav(output_filename1, BUF_SIZE, output_file_buffer1, S_RATE);
         write_wav(output_filename2, BUF_SIZE, output_file_buffer2, S_RATE);
         printf("\nWrote: %s and %s",output_filename1,output_filename2);

          if (input_file1) { fclose(input_file1); } // close open files
          if (input_file2) { fclose(input_file2); }
     }

     return(0);
}

/*creates an entropy pool to make the randomness more random*/
int randshuffler(int pool, int total)
{
     int z, randbuff[pool], randmid, randout;

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
     int r1, r2, rtotal;
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
