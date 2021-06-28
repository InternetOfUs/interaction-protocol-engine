/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
