/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import java.util.List;

import eu.internetofus.common.components.Model;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Contains the found published norms.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(description = "Contains a set of published norms found")
public class PublishedNormsPage extends ReflectionModel implements Model {

  /**
   * The index of the first community returned.
   */
  @Schema(description = "The index of the first published norm returned.", example = "0")
  public int offset;

  /**
   * The number total of communities that satisfies the search.
   */
  @Schema(description = "The number total of published norms that satisfies the search.", example = "100")
  public long total;

  /**
   * The published norms.
   */
  @ArraySchema(schema = @Schema(implementation = PublishedNorm.class), arraySchema = @Schema(description = "The set of published norms found"))
  public List<PublishedNorm> norms;

}
