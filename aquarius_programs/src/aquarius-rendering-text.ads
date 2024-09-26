with Aquarius.Streams;

package Aquarius.Rendering.Text is

   function Text_Renderer return Aquarius_Renderer;
   function File_Renderer
     (Path : String)
      return Aquarius_Renderer;

   function Stream_Renderer
     (Stream : Aquarius.Streams.Writer_Reference)
      return Aquarius_Renderer;

end Aquarius.Rendering.Text;
