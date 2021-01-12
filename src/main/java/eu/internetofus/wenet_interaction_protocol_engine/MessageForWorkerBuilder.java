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

import eu.internetofus.common.components.incentive_server.Incentive;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import io.vertx.core.json.JsonObject;

/**
 * Component used to create the messages that can be wend to a worker.
 *
 * @see HardCodedProtocolWorker
 * @see EngineWorker
 *
 * @author UDT-IA, IIIA-CSIC
 */
public interface MessageForWorkerBuilder {

  /**
   * The types of messages that the worker can receive.
   */
  public enum Type {
    /**
     * When the message inform that a task is created.
     */
    CREATED_TASK,

    /**
     * When the message inform about a task transaction that has to be done.
     */
    DO_TASK_TRANSACTION,

    /**
     * When the message inform about an incentive to inform to the user.
     */
    SEND_INCENTIVE;

  }

  /**
   * Create the message to inform that a task is created.
   *
   * @param task that has been created.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildCreatedTaskMessage(final Task task) {

    return new JsonObject().put("type", Type.CREATED_TASK.name()).put("task", task.toJsonObject());

  }

  /**
   * Create the message to inform about a task transaction that has to be done.
   *
   * @param transaction to do.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildDoTaskTransactionMessage(final TaskTransaction transaction) {

    return new JsonObject().put("type", Type.DO_TASK_TRANSACTION.name()).put("transaction", transaction.toJsonObject());

  }

  /**
   * Create the message to inform about an incentive to inform to the user.
   *
   * @param incentive to send.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildSendIncentiveMessage(final Incentive incentive) {

    return new JsonObject().put("type", Type.SEND_INCENTIVE.name()).put("incentive", incentive.toJsonObject());

  }

}