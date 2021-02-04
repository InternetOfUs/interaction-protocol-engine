/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 1994 - 2021 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
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

import static org.mockito.Mockito.doReturn;

import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Test the {@link HardCodedProtocolWorker}
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith({ VertxExtension.class, MockitoExtension.class })
public class HardCodedProtocolWorkerTest {

  /**
   * Check that capture exception when handle message.
   */
  @Test
  public void shouldCaptureException() {

    final var worker = new HardCodedProtocolWorker();
    worker.handle(null);
  }

  /**
   * Check that can manage a bad send incentive message.
   *
   * @param msg to process.
   */
  @Test
  public void shouldCaptureBadIncentiveMessage(@Mock final Message<JsonObject> msg) {

    final var worker = new HardCodedProtocolWorker();
    final var body = new JsonObject();
    body.put("type", MessageForWorkerBuilder.Type.SEND_INCENTIVE.name());
    doReturn(body).when(msg).body();
    worker.handle(msg);
  }

  /**
   * Check that can manage a bad created task message.
   *
   * @param msg to process.
   */
  @Test
  public void shouldCaptureBadTaskMessage(@Mock final Message<JsonObject> msg) {

    final var worker = new HardCodedProtocolWorker();
    final var body = new JsonObject();
    body.put("type", MessageForWorkerBuilder.Type.CREATED_TASK.name());
    doReturn(body).when(msg).body();
    worker.handle(msg);
  }

  /**
   * Check that can manage a bad do task transaction message.
   *
   * @param msg to process.
   */
  @Test
  public void shouldCaptureBadTaskTransactionMessage(@Mock final Message<JsonObject> msg) {

    final var worker = new HardCodedProtocolWorker();
    final var body = new JsonObject();
    body.put("type", MessageForWorkerBuilder.Type.DO_TASK_TRANSACTION.name());
    doReturn(body).when(msg).body();
    worker.handle(msg);
  }

//  /**
//   * Check that can create environment with task without attributes.
//   */
//  @Test
//  public void shouldCreateEnvironemntWithoutAttributes() {
//
//    final var task = new Task();
//    final var env = new HardCodedProtocolWorker.HardCodedProtocolEnvironment(task, "http://callback");
//    final var body = new JsonObject();
//    body.put("type", MessageForWorkerBuilder.Type.DO_TASK_TRANSACTION.name());
//    assertThat(task.attributes).isNotNull();
//    assertThat(env.callbackUrl).isEqualTo("http://callback");
//
//  }

}
