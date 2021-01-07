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

package eu.internetofus.wenet_interaction_protocol_engine;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.json.JsonArray;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link HardCodedProtocolWorker}.
 *
 * @see HardCodedProtocolWorker
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class HardCodedProtocolWorkerTest {

  /**
   * Should for the task attributes.
   *
   * @param testContext context for the tests.
   */
  @Test
  public void shouldFixTaskAttributes(final VertxTestContext testContext) {

    final var worker = new HardCodedProtocolWorker();
    final var taskId = UUID.randomUUID().toString();
    testContext.assertComplete(worker.createEnvironmentFor(taskId)).onComplete(testContext.succeeding(env -> testContext.verify(() -> {

      assertThat(env.task).isNotNull();
      assertThat(env.task.id).isEqualTo(taskId);
      assertThat(env.task.attributes).isNotNull();
      assertThat(env.task.attributes.getJsonArray("unanswered")).isEqualTo(new JsonArray());
      assertThat(env.task.attributes.getJsonArray("volunteers")).isEqualTo(new JsonArray());
      assertThat(env.task.attributes.getJsonArray("declined")).isEqualTo(new JsonArray());
      assertThat(env.task.attributes.getJsonArray("accepted")).isEqualTo(new JsonArray());
      assertThat(env.task.attributes.getJsonArray("refused")).isEqualTo(new JsonArray());
      testContext.completeNow();

    })));

  }

}
